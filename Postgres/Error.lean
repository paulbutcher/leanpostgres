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

/-- Whether a constraint name's character is percent-encoded: those that would end or split the
bracketed prefix, {lit}`%`, {lit}`;`, {lit}`]` and anything up to space. All are ASCII. -/
private def escaped (c : Char) : Bool :=
  c == '%' || c == ';' || c == ']' || c ≤ ' '

/-- The uppercase hexadecimal digit for {lean}`n`, which is less than 16. -/
private def hexChar (n : Nat) : Char :=
  if n < 10 then Char.ofNat ('0'.toNat + n) else Char.ofNat ('A'.toNat + n - 10)

/-- The value of an uppercase hexadecimal digit. -/
private def hexDigit (c : Char) : Option Nat :=
  if '0' ≤ c && c ≤ '9' then some (c.toNat - '0'.toNat)
  else if 'A' ≤ c && c ≤ 'F' then some (c.toNat - 'A'.toNat + 10)
  else none

private def encodeChars : List Char → List Char
  | [] => []
  | c :: cs =>
    if escaped c then '%' :: hexChar (c.toNat / 16) :: hexChar (c.toNat % 16) :: encodeChars cs
    else c :: encodeChars cs

private def decodeChars : List Char → Option (List Char)
  | [] => some []
  | c :: cs =>
    if c = '%' then
      match cs with
      | h :: l :: rest => do
        let hi ← hexDigit h
        let lo ← hexDigit l
        return Char.ofNat (hi * 16 + lo) :: (← decodeChars rest)
      | _ => none
    else (c :: ·) <$> decodeChars cs

/-- {lean}`s` with the characters that would end or split the bracketed prefix percent-encoded, as
the bindings do. -/
private def encodeField (s : String) : String := String.ofList (encodeChars s.toList)

/-- Undoes {name}`encodeField`. -/
private def decodeField (s : String) : Option String := String.ofList <$> decodeChars s.toList

private theorem hexDigit_hexChar : ∀ n < 16, hexDigit (hexChar n) = some n := by decide

private theorem toNat_lt_of_escaped {c : Char} (h : escaped c = true) : c.toNat < 256 := by
  simp only [escaped, Bool.or_eq_true, beq_iff_eq, decide_eq_true_eq] at h
  rcases h with ((rfl | rfl) | rfl) | h
  · decide
  · decide
  · decide
  · exact Nat.lt_of_le_of_lt (show c.toNat ≤ 32 from h) (by decide)

private theorem decodeChars_encodeChars (cs : List Char) : decodeChars (encodeChars cs) = some cs := by
  induction cs with
  | nil => rfl
  | cons c cs ih =>
    unfold encodeChars
    by_cases h : escaped c
    · have lt := toNat_lt_of_escaped h
      simp [h, decodeChars, hexDigit_hexChar (c.toNat / 16) (by omega),
        hexDigit_hexChar (c.toNat % 16) (by omega), ih, Nat.div_add_mod', Char.ofNat_toNat]
    · have : c ≠ '%' := by rintro rfl; exact h rfl
      simp only [h, Bool.false_eq_true, ↓reduceIte]
      rw [decodeChars.eq_def]
      simp only [this, ↓reduceIte, ih]
      rfl

/-- {name}`decodeField` recovers whatever {name}`encodeField` encoded. -/
private theorem decodeField_encodeField (s : String) : decodeField (encodeField s) = some s := by
  simp [decodeField, encodeField, String.toList_ofList, decodeChars_encodeChars]

@[no_expose] instance : ToString Error where
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
