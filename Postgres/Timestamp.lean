module
import all Postgres.TemporalCodec
public import Postgres.TemporalCodec
public import Postgres.QueryParam
public import Postgres.QueryResult

set_option doc.verso true
set_option linter.missingDocs true

namespace Postgres

public section

/-- Renders {lit}`t` as Postgres {lit}`timestamp` text ({lit}`YYYY-MM-DD HH:MM:SS[.ffffff]`). -/
public def timestampToText (t : Std.Time.PlainDateTime) : String :=
  s!"{TemporalCodec.dateText t.date} {TemporalCodec.timeText t.time}"

/-- Parses Postgres {lit}`timestamp` text ({lit}`YYYY-MM-DD HH:MM:SS[.ffffff]`). -/
public def timestampOfText? (s : String) : Option Std.Time.PlainDateTime := do
  let (date, rest1) ← TemporalCodec.parsePlainDate? s.toList
  let rest2 ← match rest1 with | ' ' :: r => some r | _ => none
  let (time, rest3) ← TemporalCodec.parsePlainTime? rest2
  if rest3 == [] then some { date, time } else none

public instance : QueryParam Std.Time.PlainDateTime where
  bind stmt i t := stmt.bindText i (timestampToText t)

public instance : ResultColumn Std.Time.PlainDateTime where
  get stmt i := do
    let s ← stmt.columnText i
    match timestampOfText? s with
    | some t => return t
    | none => throw <| IO.userError s!"column {i}: expected a timestamp literal, got: {s}"

end
end Postgres
