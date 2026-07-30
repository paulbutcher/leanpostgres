module

set_option doc.verso true
set_option linter.missingDocs true

namespace Postgres.TextCodec

private def hexDigit (n : UInt8) : Char :=
  if n < 10 then Char.ofNat (n.toNat + '0'.toNat)
  else Char.ofNat (n.toNat - 10 + 'a'.toNat)

private def hexValue? (c : Char) : Option UInt8 :=
  if '0' ≤ c && c ≤ '9' then some (c.toNat - '0'.toNat).toUInt8
  else if 'a' ≤ c && c ≤ 'f' then some (c.toNat - 'a'.toNat + 10).toUInt8
  else if 'A' ≤ c && c ≤ 'F' then some (c.toNat - 'A'.toNat + 10).toUInt8
  else none

/-- Encodes {lit}`bytes` as a Postgres {lit}`\x`-prefixed {lit}`bytea` hex literal. -/
private def byteArrayToHex (bytes : ByteArray) : String :=
  let appendByte (acc : String) (b : UInt8) : String :=
    acc.push (hexDigit (b >>> 4)) |>.push (hexDigit (b &&& 0xF))
  "\\x" ++ bytes.toList.foldl appendByte ""

/-- Decodes a Postgres {lit}`\x`-prefixed {lit}`bytea` hex literal. -/
private def hexToByteArray? (s : String) : Option ByteArray := do
  let body ← if s.startsWith "\\x" then some (s.drop 2).toString else none
  let rec pairs : List Char → Option (List UInt8)
    | [] => some []
    | [_] => none
    | hi :: lo :: rest => return ((← hexValue? hi) <<< 4 ||| (← hexValue? lo)) :: (← pairs rest)
  return ByteArray.mk (← pairs body.toList).toArray

/--
Renders {lit}`f` as the exact decimal value of its IEEE 754 bit pattern (not the shortest
round-tripping decimal), so that parsing the result back always reproduces the original bits —
Postgres accepts arbitrarily long numeric literals for {lit}`real`/{lit}`double precision`, so
there's no need for a shortest-decimal algorithm here.
-/
private def floatToExactDecimal (f : Float) : String :=
  let bits := f.toBits
  let sign := bits >>> 63 == 1
  let expField := ((bits >>> 52) &&& 0x7FF).toNat
  let mantField := (bits &&& 0xFFFFFFFFFFFFF).toNat
  if expField == 0 && mantField == 0 then
    if sign then "-0" else "0"
  else
    -- Normal: value = (2^52 + mantissa) * 2^(exp - 1075). Subnormal: value = mantissa * 2^-1074.
    let (numerator, e2) :=
      if expField == 0 then (mantField, (-1074 : Int))
      else (mantField + (1 <<< 52), (expField : Int) - 1075)
    let magnitude :=
      if e2 ≥ 0 then
        toString (numerator * (2 ^ e2.toNat))
      else
        -- x / 2^k = (x * 5^k) / 10^k, which terminates in exactly k decimal digits.
        let k := (-e2).toNat
        let scaled := numerator * (5 ^ k)
        let digits := toString scaled
        let digits :=
          if digits.length ≤ k then
            String.ofList (List.replicate (k + 1 - digits.length) '0') ++ digits
          else digits
        let intPart := (digits.dropEnd k).toString
        let fracPart := (digits.takeEnd k).toString
        s!"{if intPart.isEmpty then "0" else intPart}.{fracPart}"
    if sign then s!"-{magnitude}" else magnitude

/-- Encodes {lit}`f` as Postgres text, including {lit}`NaN`/{lit}`Infinity`/{lit}`-Infinity`. -/
private def floatToText (f : Float) : String :=
  if f.isNaN then "NaN"
  else if f.isInf then (if f < 0 then "-Infinity" else "Infinity")
  else floatToExactDecimal f

private def digitsToNat? (ds : List Char) : Option Nat :=
  if ds.isEmpty || !ds.all Char.isDigit then none
  else some (ds.foldl (fun acc c => acc * 10 + (c.toNat - '0'.toNat)) 0)

/-- Parses the sign and digits of a literal exponent suffix (e.g. {lit}`+3`, {lit}`-12`). -/
private def parseSignedExponent? (s : String) : Option Int :=
  match s.toList with
  | '-' :: rest => (fun n => -(Int.ofNat n)) <$> digitsToNat? rest
  | '+' :: rest => Int.ofNat <$> digitsToNat? rest
  | rest => Int.ofNat <$> digitsToNat? rest

/-- Parses the unsigned body (no leading {lit}`+`/{lit}`-`) of a Postgres float literal. -/
private def parseUnsignedFloatBody? (s : String) : Option Float := do
  let normalized := s.map (fun c => if c == 'E' then 'e' else c)
  let (mantissaPart, expPart) ←
    match normalized.splitOn "e" with
    | [m] => some (m, none)
    | [m, e] => some (m, some e)
    | _ => none
  if mantissaPart.isEmpty then none else
  let (intPart, fracPart) ←
    match mantissaPart.splitOn "." with
    | [i] => some (i, "")
    | [i, f] => some (i, f)
    | _ => none
  let mantissa ← digitsToNat? (intPart ++ fracPart).toList
  let fracLen : Int := fracPart.length
  let litExp ← match expPart with
    | none => some (0 : Int)
    | some e => parseSignedExponent? e
  let net := fracLen - litExp
  return if net ≥ 0 then Float.ofScientific mantissa true net.toNat
    else Float.ofScientific mantissa false (-net).toNat

/-- Parses Postgres float text, including {lit}`NaN`/{lit}`Infinity`/{lit}`-Infinity`. -/
private def floatOfText? (s : String) : Option Float :=
  if s == "NaN" then some (0.0 / 0.0)
  else if s == "Infinity" then some (1.0 / 0.0)
  else if s == "-Infinity" then some (-(1.0 / 0.0))
  else
    match s.toList with
    | '-' :: rest => Neg.neg <$> parseUnsignedFloatBody? (String.ofList rest)
    | '+' :: rest => parseUnsignedFloatBody? (String.ofList rest)
    | _ => parseUnsignedFloatBody? s

end Postgres.TextCodec
