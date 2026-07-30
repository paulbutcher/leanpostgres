module
import all Postgres.TemporalCodec
public import Postgres.TemporalCodec
public import Postgres.QueryParam
public import Postgres.QueryResult

set_option doc.verso true
set_option linter.missingDocs true

namespace Postgres

public section

/--
A Postgres {lit}`time`/{lit}`time with time zone` value.

{lit}`offset` is {lean}`none` for a plain {lit}`time` (no zone) and {lean}`some` a UTC offset for
{lit}`timetz` — which of the two is meant is determined entirely by whether the caller sets this
field, not by any separate type. There's no bundled {lit}`Std.Time` type combining a bare
time-of-day with an optional zone, so this composes {lit}`Std.Time`'s own
{name (full := Std.Time.PlainTime)}`PlainTime` and {name (full := Std.Time.TimeZone.Offset)}`Offset`
rather than duplicating either.
-/
public structure Time where
  /-- The time-of-day. -/
  time : Std.Time.PlainTime
  /-- The UTC offset, present only for {lit}`timetz` values. -/
  offset : Option Std.Time.TimeZone.Offset := none
  deriving Repr, BEq, Inhabited

/--
Renders {lit}`t` as Postgres text: {lit}`HH:MM:SS[.ffffff]`, plus a UTC-offset suffix if
{lit}`offset` is set.
-/
public def Time.toText (t : Time) : String :=
  let base := TemporalCodec.timeText t.time
  match t.offset with
  | none => base
  | some off => base ++ TemporalCodec.formatOffsetSeconds off.second.val

/-- Parses Postgres {lit}`time`/{lit}`timetz` text. -/
public def Time.ofText? (s : String) : Option Time := do
  let (time, rest) ← TemporalCodec.parsePlainTime? s.toList
  match rest with
  | [] => some { time, offset := none }
  | _ =>
    let secs ← TemporalCodec.parseOffsetSeconds? rest
    some { time, offset := some (Std.Time.TimeZone.Offset.ofSeconds (Std.Time.Second.Offset.ofInt secs)) }

public instance : QueryParam Time where
  bind stmt i t := stmt.bindText i t.toText

public instance : ResultColumn Time where
  get stmt i := do
    let s ← stmt.columnText i
    match Time.ofText? s with
    | some t => return t
    | none => throw <| IO.userError s!"column {i}: expected a time literal, got: {s}"

end
end Postgres
