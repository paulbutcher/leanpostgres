/-
Copyright (c) 2026 Paul Butcher. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module
import all Postgres.TextCodec
public import Postgres.TextCodec
public import Postgres.QueryParam
public import Postgres.QueryResult

set_option doc.verso true
set_option linter.missingDocs true

namespace Postgres

public section

/--
Pure text encode/decode for a 1-D array's element type. {name}`QueryParam`/{name}`ResultColumn`
only expose {lit}`Stmt`-bound binding/reading, not a standalone text conversion; this fills that
gap for the scalar types array literals are built from.
-/
public class ArrayElem (α : Type) where
  /-- Encodes {name}`α` as Postgres text (unquoted; array-literal quoting is applied separately). -/
  encode : α → String
  /-- Decodes {name}`α` from Postgres text (already unquoted/unescaped). -/
  decode? : String → Option α

public instance : ArrayElem String where
  encode := id
  decode? := some

public instance : ArrayElem Bool where
  encode b := if b then "t" else "f"
  decode?
    | "t" => some true
    | "f" => some false
    | _ => none

public instance : ArrayElem Int16 where
  encode := toString
  decode? s := do
    let n ← s.toInt?
    if Int16.minValue.toInt ≤ n && n ≤ Int16.maxValue.toInt then some (Int16.ofInt n) else none

public instance : ArrayElem Int32 where
  encode := toString
  decode? s := do
    let n ← s.toInt?
    if Int32.minValue.toInt ≤ n && n ≤ Int32.maxValue.toInt then some (Int32.ofInt n) else none

public instance : ArrayElem Int64 where
  encode := toString
  decode? s := do
    let n ← s.toInt?
    if Int64.minValue.toInt ≤ n && n ≤ Int64.maxValue.toInt then some (Int64.ofInt n) else none

public instance : ArrayElem Float32 where
  encode f := TextCodec.floatToText f.toFloat
  decode? s := Float.toFloat32 <$> TextCodec.floatOfText? s

public instance : ArrayElem Float where
  encode := TextCodec.floatToText
  decode? := TextCodec.floatOfText?

public instance : ArrayElem ByteArray where
  encode := TextCodec.byteArrayToHex
  decode? := TextCodec.hexToByteArray?

/-- Whether {name}`s` needs {lit}`"…"` quoting to appear as an array-literal element. -/
private def needsQuoting (s : String) : Bool :=
  s.isEmpty || s == "NULL" ||
    s.toList.any (fun c => c == ',' || c == '{' || c == '}' || c == '"' || c == '\\' || c.isWhitespace)

private def escapeForArray (s : String) : String :=
  String.ofList (s.toList.foldr (fun c acc => if c == '"' || c == '\\' then '\\' :: c :: acc else c :: acc) [])

private def formatElement (s : String) : String :=
  if needsQuoting s then "\"" ++ escapeForArray s ++ "\"" else s

/-- Renders array elements ({lit}`none` for {lit}`NULL`) as a Postgres {lit}`{a,b,c}` array literal. -/
public def arrayLiteralOf (elems : List (Option String)) : String :=
  "{" ++ String.intercalate "," (elems.map (fun | none => "NULL" | some s => formatElement s)) ++ "}"

/-- Single-pass array-literal element scanner. -/
private def scanArrayElements
    (chars : List Char) (elems : List (Option String)) (cur : List Char)
    (quoted everQuoted escaping : Bool) : Option (List (Option String)) :=
  match chars with
  | [] => none
  | c :: rest =>
    if escaping then
      scanArrayElements rest elems (c :: cur) quoted everQuoted false
    else if quoted then
      match c with
      | '\\' => scanArrayElements rest elems cur quoted everQuoted true
      | '"' => scanArrayElements rest elems cur false everQuoted false
      | _ => scanArrayElements rest elems (c :: cur) quoted everQuoted false
    else
      match c with
      | '"' =>
        if cur.isEmpty && !everQuoted then scanArrayElements rest elems cur true true false else none
      | ',' =>
        let tok := String.ofList cur.reverse
        let elem := if everQuoted then some tok else if tok == "NULL" then none else some tok
        scanArrayElements rest (elem :: elems) [] false false false
      | '}' =>
        if rest == [] then
          let tok := String.ofList cur.reverse
          let elem := if everQuoted then some tok else if tok == "NULL" then none else some tok
          some (elem :: elems).reverse
        else none
      | _ => scanArrayElements rest elems (c :: cur) quoted everQuoted false

/-- Parses a Postgres {lit}`{a,b,c}` array literal into its elements ({lean}`none` for {lit}`NULL`). -/
public def parseArrayLiteral? (s : String) : Option (List (Option String)) :=
  match s.toList with
  | '{' :: '}' :: [] => some []
  | '{' :: rest => scanArrayElements rest [] [] false false false
  | _ => none

public instance [ArrayElem α] : QueryParam (Array α) where
  bind stmt i arr := stmt.bindText i (arrayLiteralOf (arr.toList.map (some ∘ ArrayElem.encode)))

public instance [ArrayElem α] : QueryParam (Array (Option α)) where
  bind stmt i arr := stmt.bindText i (arrayLiteralOf (arr.toList.map (Option.map ArrayElem.encode)))

public instance [ArrayElem α] : ResultColumn (Array α) where
  get stmt i := do
    let s ← stmt.columnText i
    match parseArrayLiteral? s with
    | none => throw <| IO.userError s!"column {i}: expected a Postgres array literal, got: {s}"
    | some elems =>
      let decoded ← elems.mapM fun
        | none => throw <| IO.userError s!"column {i}: array contains NULL but element type is non-nullable"
        | some raw =>
          match ArrayElem.decode? raw with
          | some v => pure v
          | none => throw <| IO.userError s!"column {i}: could not decode array element: {raw}"
      return decoded.toArray

public instance [ArrayElem α] : ResultColumn (Array (Option α)) where
  get stmt i := do
    let s ← stmt.columnText i
    match parseArrayLiteral? s with
    | none => throw <| IO.userError s!"column {i}: expected a Postgres array literal, got: {s}"
    | some elems =>
      let decoded ← elems.mapM fun
        | none => pure none
        | some raw =>
          match ArrayElem.decode? raw with
          | some v => pure (some v)
          | none => throw <| IO.userError s!"column {i}: could not decode array element: {raw}"
      return decoded.toArray

end
end Postgres
