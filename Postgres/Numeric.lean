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
A faithful carrier for Postgres's arbitrary-precision {lit}`numeric`/{lit}`decimal` type.

{lit}`ofDigits`'s {lit}`unscaled`/{lit}`scale` pair is the unscaled integer value and the number
of digits after the decimal point (so the represented value is {lit}`unscaled * 10 ^ (-scale)`),
which lets round-tripping preserve trailing zeros exactly as Postgres printed them (e.g.
{lit}`123.4500`). No arithmetic is provided on {name}`Numeric` itself in v1 — use {lit}`toFloat`/
{lit}`toInt?` for lossy conversions where that's acceptable to the caller.
-/
public inductive Numeric where
  /-- A finite value: {lit}`unscaled * 10 ^ (-scale)`, negated if {lit}`negative`. -/
  | ofDigits (negative : Bool) (unscaled : Nat) (scale : Nat)
  /-- Postgres's {lit}`NaN` numeric value. -/
  | nan
  /-- Postgres's {lit}`Infinity` numeric value. -/
  | posInf
  /-- Postgres's {lit}`-Infinity` numeric value. -/
  | negInf
  deriving Repr, BEq, Inhabited

/-- Renders {lit}`n` as Postgres {lit}`numeric` text. -/
public def Numeric.toText : Numeric → String
  | .nan => "NaN"
  | .posInf => "Infinity"
  | .negInf => "-Infinity"
  | .ofDigits negative unscaled scale =>
    let digits := toString unscaled
    let digits :=
      if digits.length ≤ scale then
        String.ofList (List.replicate (scale + 1 - digits.length) '0') ++ digits
      else digits
    let body :=
      if scale == 0 then digits
      else
        let intPart := (digits.dropEnd scale).toString
        let fracPart := (digits.takeEnd scale).toString
        s!"{intPart}.{fracPart}"
    if negative then s!"-{body}" else body

private def parseUnsignedNumericBody? (s : String) : Option (Nat × Nat) := do
  let (intPart, fracPart) ←
    match s.splitOn "." with
    | [i] => some (i, "")
    | [i, f] => some (i, f)
    | _ => none
  let unscaled ← TextCodec.digitsToNat? (intPart ++ fracPart).toList
  return (unscaled, fracPart.length)

/-- Parses Postgres {lit}`numeric` text, including {lit}`NaN`/{lit}`Infinity`/{lit}`-Infinity`. -/
public def Numeric.ofText? (s : String) : Option Numeric :=
  if s == "NaN" then some .nan
  else if s == "Infinity" then some .posInf
  else if s == "-Infinity" then some .negInf
  else
    match s.toList with
    | '-' :: rest =>
      (fun (u, scale) => Numeric.ofDigits true u scale) <$> parseUnsignedNumericBody? (String.ofList rest)
    | _ =>
      (fun (u, scale) => Numeric.ofDigits false u scale) <$> parseUnsignedNumericBody? s

/--
Converts to a {name}`Float`, lossily for values whose precision exceeds a {lean}`Float`'s.
-/
public def Numeric.toFloat : Numeric → Float
  | .nan => 0.0 / 0.0
  | .posInf => 1.0 / 0.0
  | .negInf => -(1.0 / 0.0)
  | .ofDigits negative unscaled scale =>
    let magnitude := Float.ofScientific unscaled true scale
    if negative then -magnitude else magnitude

/--
Converts to an {name}`Int`, truncating any fractional digits.

Returns {lean}`none` for {lit}`NaN`/{lit}`Infinity`/{lit}`-Infinity`, which have no integer value.
-/
public def Numeric.toInt? : Numeric → Option Int
  | .nan => none
  | .posInf => none
  | .negInf => none
  | .ofDigits negative unscaled scale =>
    let whole := unscaled / (10 ^ scale)
    some (if negative then -(Int.ofNat whole) else Int.ofNat whole)

public instance : QueryParam Numeric where
  bind stmt i n := stmt.bindText i n.toText

public instance : ResultColumn Numeric where
  get stmt i := do
    let s ← stmt.columnText i
    match Numeric.ofText? s with
    | some n => return n
    | none => throw <| IO.userError s!"column {i}: expected a numeric literal, got: {s}"

end
end Postgres
