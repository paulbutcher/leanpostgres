module
import all Postgres.TextCodec
public import Postgres.TextCodec
public import Std.Time

set_option doc.verso true
set_option linter.missingDocs true

namespace Postgres.TemporalCodec

private def padNat (width : Nat) (n : Nat) : String :=
  let s := toString n
  if s.length ≥ width then s else String.ofList (List.replicate (width - s.length) '0') ++ s

private def parseFixedDigits? (width : Nat) (cs : List Char) : Option (Nat × List Char) :=
  let rec go (n : Nat) (acc : Nat) (cs : List Char) : Option (Nat × List Char) :=
    match n with
    | 0 => some (acc, cs)
    | n + 1 =>
      match cs with
      | c :: rest => if c.isDigit then go n (acc * 10 + (c.toNat - '0'.toNat)) rest else none
      | [] => none
  go width 0 cs

private def parseDigitRun (acc : List Char) : List Char → (List Char × List Char)
  | c :: rest => if c.isDigit then parseDigitRun (c :: acc) rest else (acc.reverse, c :: rest)
  | [] => (acc.reverse, [])

/-- Parses a Postgres date's {lit}`YYYY(+)-MM-DD` prefix, returning {lit}`(year, month, day, rest)`. -/
private def parseDateParts? (cs : List Char) : Option (Nat × Nat × Nat × List Char) := do
  let (yearDigits, r1) := parseDigitRun [] cs
  let year ← TextCodec.digitsToNat? yearDigits
  let r2 ← match r1 with | '-' :: r => some r | _ => none
  let (month, r3) ← parseFixedDigits? 2 r2
  let r4 ← match r3 with | '-' :: r => some r | _ => none
  let (day, r5) ← parseFixedDigits? 2 r4
  return (year, month, day, r5)

/--
Parses a Postgres time-of-day's {lit}`HH:MM:SS[.ffffff]` prefix, returning
{lit}`(hour, minute, second, microsecond, rest)`. Accepts any number of fractional digits
(Postgres trims trailing zeros on output), padding/truncating to microsecond precision.
-/
private def parseTimeOfDay? (cs : List Char) : Option (Nat × Nat × Nat × Nat × List Char) := do
  let (hour, r1) ← parseFixedDigits? 2 cs
  let r2 ← match r1 with | ':' :: r => some r | _ => none
  let (minute, r3) ← parseFixedDigits? 2 r2
  let r4 ← match r3 with | ':' :: r => some r | _ => none
  let (second, r5) ← parseFixedDigits? 2 r4
  match r5 with
  | '.' :: r6 =>
    let (fracDigits, r7) := parseDigitRun [] r6
    let padded :=
      if fracDigits.length ≥ 6 then fracDigits.take 6
      else fracDigits ++ List.replicate (6 - fracDigits.length) '0'
    let micro ← TextCodec.digitsToNat? padded
    return (hour, minute, second, micro, r7)
  | _ => return (hour, minute, second, 0, r5)

/--
Parses a Postgres UTC-offset suffix (e.g. {lit}`+05:30`, {lit}`-04`, {lit}`+00`), consuming
{lit}`cs` completely. Returns the offset as a signed number of seconds.
-/
private def parseOffsetSeconds? (cs : List Char) : Option Int := do
  match cs with
  | sign :: rest =>
    if sign != '+' && sign != '-' then none
    else
      let (hour, r1) ← parseFixedDigits? 2 rest
      let (minute, r2) ← match r1 with
        | ':' :: r => parseFixedDigits? 2 r
        | [] => some (0, ([] : List Char))
        | _ => none
      let (second, r3) ← match r2 with
        | ':' :: r => parseFixedDigits? 2 r
        | [] => some (0, ([] : List Char))
        | _ => none
      if r3 != [] then none
      else
        let total : Int := Int.ofNat (hour * 3600 + minute * 60 + second)
        return if sign == '-' then -total else total
  | [] => none

/-- Renders a signed offset (in seconds) as {lit}`±HH[:MM[:SS]]`, matching Postgres's output. -/
private def formatOffsetSeconds (totalSeconds : Int) : String :=
  let sign := if totalSeconds < 0 then "-" else "+"
  let abs := totalSeconds.natAbs
  let hour := abs / 3600
  let rem := abs % 3600
  let minute := rem / 60
  let second := rem % 60
  if second != 0 then s!"{sign}{padNat 2 hour}:{padNat 2 minute}:{padNat 2 second}"
  else if minute != 0 then s!"{sign}{padNat 2 hour}:{padNat 2 minute}"
  else s!"{sign}{padNat 2 hour}"

/--
Safely constructs a {name (full := Std.Time.PlainDate)}`PlainDate`, validating {lit}`month`/{lit}`day`
against the calendar (leap years included) rather than trusting raw text.
-/
private def mkDate? (year : Int) (month day : Nat) : Option Std.Time.PlainDate := do
  let m ← Std.Time.Internal.Bounded.LE.ofInt (lo := 1) (hi := 12) (Int.ofNat month)
  let d ← Std.Time.Internal.Bounded.LE.ofInt (lo := 1) (hi := 31) (Int.ofNat day)
  Std.Time.PlainDate.ofYearMonthDay? year m d

/-- Renders {lit}`d` as Postgres {lit}`date` text ({lit}`YYYY-MM-DD`). -/
private def dateText (d : Std.Time.PlainDate) : String :=
  s!"{padNat 4 d.year.toNat}-{padNat 2 d.month.val.toNat}-{padNat 2 d.day.val.toNat}"

/-- Safely constructs a {name (full := Std.Time.PlainTime)}`PlainTime` from its raw components. -/
private def mkTime? (hour minute second microsecond : Nat) : Option Std.Time.PlainTime := do
  let h ← Std.Time.Internal.Bounded.LE.ofInt (lo := 0) (hi := 23) (Int.ofNat hour)
  let m ← Std.Time.Internal.Bounded.LE.ofInt (lo := 0) (hi := 59) (Int.ofNat minute)
  let s ← Std.Time.Internal.Bounded.LE.ofInt (lo := 0) (hi := 60) (Int.ofNat second)
  let n ← Std.Time.Internal.Bounded.LE.ofInt (lo := 0) (hi := 999999999) (Int.ofNat (microsecond * 1000))
  some (Std.Time.PlainTime.ofHourMinuteSecondsNano h m s n)

/-- Renders {lit}`t` as Postgres text: {lit}`HH:MM:SS`, plus a {lit}`.ffffff` suffix if nonzero. -/
private def timeText (t : Std.Time.PlainTime) : String :=
  let micro := t.nanosecond.val.toNat / 1000
  let base := s!"{padNat 2 t.hour.val.toNat}:{padNat 2 t.minute.val.toNat}:{padNat 2 t.second.val.toNat}"
  if micro == 0 then base else s!"{base}.{padNat 6 micro}"

/-- Parses a Postgres date prefix into a validated {name (full := Std.Time.PlainDate)}`PlainDate`. -/
private def parsePlainDate? (cs : List Char) : Option (Std.Time.PlainDate × List Char) := do
  let (year, month, day, rest) ← parseDateParts? cs
  let d ← mkDate? (Int.ofNat year) month day
  return (d, rest)

/-- Parses a Postgres time-of-day prefix into a {name (full := Std.Time.PlainTime)}`PlainTime`. -/
private def parsePlainTime? (cs : List Char) : Option (Std.Time.PlainTime × List Char) := do
  let (hour, minute, second, microsecond, rest) ← parseTimeOfDay? cs
  let t ← mkTime? hour minute second microsecond
  return (t, rest)

end Postgres.TemporalCodec
