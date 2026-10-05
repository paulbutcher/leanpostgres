/-
Copyright (c) 2026 Paul Butcher. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module

set_option doc.verso true
set_option linter.missingDocs true

namespace Postgres

public section

/--
A Postgres error: the five-character SQLSTATE code alongside a human-readable message, and, for a
constraint violation, the name of the constraint.

{name (full := Error.sqlstate)}`sqlstate` is empty for failures that occur before a connection
exists (e.g. a refused TCP connection), since libpq has no result object to read a SQLSTATE from
at that point. Once a connection is open, errors from executed statements carry a real SQLSTATE.
-/
structure Error where
  /-- The five-character SQLSTATE code, or the empty string when none is available. -/
  sqlstate : String
  /-- A human-readable description of the error. -/
  message : String
  /--
  The constraint a statement violated, as the server names it (class {lit}`23` SQLSTATEs), so
  that a caller can tell which of a table's unique constraints a {lit}`23505` is about without
  reading the message.
  -/
  constraint : Option String := none
deriving Repr, BEq, Inhabited

namespace Error

/-- The value of an uppercase hexadecimal digit. -/
def hexDigit (c : Char) : Option Nat :=
  if '0' ≤ c && c ≤ '9' then some (c.toNat - '0'.toNat)
  else if 'A' ≤ c && c ≤ 'F' then some (c.toNat - 'A'.toNat + 10)
  else none

/-- The bytes that would end or split the bracketed prefix, percent-encoded, as the bindings do. -/
def encodeField (s : String) : String :=
  String.join (s.toUTF8.toList.map fun b =>
    if b == '%'.toUInt8 || b == ';'.toUInt8 || b == ']'.toUInt8 || b ≤ ' '.toUInt8 then
      let hex := "0123456789ABCDEF".toList
      s!"%{hex.getD (b.toNat / 16) '0'}{hex.getD (b.toNat % 16) '0'}"
    else (Char.ofNat b.toNat).toString)

/-- Undoes {name}`encodeField`. -/
def decodeField (s : String) : Option String :=
  let rec go : List Char → List UInt8 → Option (List UInt8)
    | [], acc => some acc.reverse
    | '%' :: h :: l :: rest, acc =>
      match hexDigit h, hexDigit l with
      | some hi, some lo => go rest ((hi * 16 + lo).toUInt8 :: acc)
      | _, _ => none
    | '%' :: _, _ => none
    | c :: rest, acc => go rest (c.toString.toUTF8.toList.reverse ++ acc)
  (go s.toList []).bind fun bytes => String.fromUTF8? ⟨bytes.toArray⟩

instance : ToString Error where
  toString e :=
    let fields := match e.constraint with
      | some name => s!";constraint={encodeField name}"
      | none => ""
    s!"[{e.sqlstate}{fields}] {e.message}"

/--
Recovers the {name}`Error` carried by an {name (full := IO.Error)}`IO.Error`, if it was thrown
by this library (identified by {name}`ToString.toString`'s {lit}`[sqlstate] message` format, with
any diagnostic fields after the SQLSTATE).
Returns {lean}`none` for any other {name (full := IO.Error)}`IO.Error`, including ones from
unrelated {name}`IO` actions.
-/
def ofIOError? : IO.Error → Option Error
  | .userError msg =>
    if msg.startsWith "[" then
      match msg.splitOn "] " with
      | code :: (rest@(_ :: _)) =>
        match ((code.drop 1).toString.splitOn ";") with
        | sqlstate :: fields =>
          let constraint := fields.findSome? fun field =>
            if field.startsWith "constraint=" then decodeField (field.drop 11).toString else none
          some { sqlstate, message := String.intercalate "] " rest, constraint }
        | [] => none
      | _ => none
    else none
  | _ => none

end Error

end
end Postgres
