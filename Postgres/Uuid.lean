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

/-- A Postgres {lit}`uuid`: 16 raw bytes, formatted as the standard hyphenated hex text form. -/
public structure Uuid where
  /-- The 16 raw bytes of the UUID. -/
  bytes : ByteArray
  deriving BEq, Inhabited

/-- Renders {lit}`u` as the standard {lit}`8-4-4-4-12` hyphenated hex text form. -/
public def Uuid.toText (u : Uuid) : String :=
  let hex := (TextCodec.byteArrayToHex u.bytes).toList.drop 2
  let seg (lo hi : Nat) := String.ofList ((hex.drop lo).take (hi - lo))
  s!"{seg 0 8}-{seg 8 12}-{seg 12 16}-{seg 16 20}-{seg 20 32}"

/-- Parses the standard {lit}`8-4-4-4-12` hyphenated hex text form of a {lit}`uuid`. -/
public def Uuid.ofText? (s : String) : Option Uuid := do
  match s.splitOn "-" with
  | [p1, p2, p3, p4, p5] =>
    if p1.length == 8 && p2.length == 4 && p3.length == 4 && p4.length == 4 && p5.length == 12 then
      let bytes ← TextCodec.hexToByteArray? ("\\x" ++ p1 ++ p2 ++ p3 ++ p4 ++ p5)
      if bytes.size == 16 then some { bytes } else none
    else none
  | _ => none

public instance : QueryParam Uuid where
  bind stmt i u := stmt.bindText i u.toText

public instance : ResultColumn Uuid where
  get stmt i := do
    let s ← stmt.columnText i
    match Uuid.ofText? s with
    | some u => return u
    | none => throw <| IO.userError s!"column {i}: expected a uuid literal, got: {s}"

end
end Postgres
