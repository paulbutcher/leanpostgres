/-
Copyright (c) 2026 Paul Butcher. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
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
Renders {lit}`dt` as Postgres {lit}`timestamptz` text
({lit}`YYYY-MM-DD HH:MM:SS[.ffffff]±HH[:MM[:SS]]`).
-/
public def timestamptzToText (dt : Std.Time.DateTime) : String :=
  let pdt := dt.toPlainDateTime
  let offsetText := TemporalCodec.formatOffsetSeconds dt.timezone.offset.second.val
  s!"{TemporalCodec.dateText pdt.date} {TemporalCodec.timeText pdt.time}{offsetText}"

/--
Parses Postgres {lit}`timestamptz` text.

Postgres always normalizes {lit}`timestamptz` output to the session's {lit}`TimeZone` setting, so
the offset recovered here reflects that session setting, not necessarily the offset the value was
originally inserted with.
-/
public def timestamptzOfText? (s : String) : Option Std.Time.DateTime := do
  let (date, rest1) ← TemporalCodec.parsePlainDate? s.toList
  let rest2 ← match rest1 with | ' ' :: r => some r | _ => none
  let (time, rest3) ← TemporalCodec.parsePlainTime? rest2
  let secs ← TemporalCodec.parseOffsetSeconds? rest3
  let tz := Std.Time.TimeZone.ofSeconds "" "" (Std.Time.Second.Offset.ofInt secs)
  some (Std.Time.DateTime.ofPlainDateTimeWithZone { date, time } tz)

public instance : QueryParam Std.Time.DateTime where
  bind stmt i dt := stmt.bindText i (timestamptzToText dt)

public instance : ResultColumn Std.Time.DateTime where
  get stmt i := do
    let s ← stmt.columnText i
    match timestamptzOfText? s with
    | some dt => return dt
    | none => throw <| IO.userError s!"column {i}: expected a timestamptz literal, got: {s}"

end
end Postgres
