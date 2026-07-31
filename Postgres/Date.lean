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

/-- Renders {lit}`d` as Postgres {lit}`date` text ({lit}`YYYY-MM-DD`). -/
public def dateToText (d : Std.Time.PlainDate) : String :=
  TemporalCodec.dateText d

/--
Parses Postgres {lit}`date` text ({lit}`YYYY-MM-DD`).

BC dates are out of scope for v1; returns {lean}`none` for Postgres's {lit}`BC`-suffixed text
form.
-/
public def dateOfText? (s : String) : Option Std.Time.PlainDate := do
  let (d, rest) ← TemporalCodec.parsePlainDate? s.toList
  if rest == [] then some d else none

public instance : QueryParam Std.Time.PlainDate where
  bind stmt i d := stmt.bindText i (dateToText d)

public instance : ResultColumn Std.Time.PlainDate where
  get stmt i := do
    let s ← stmt.columnText i
    match dateOfText? s with
    | some d => return d
    | none => throw <| IO.userError s!"column {i}: expected a date literal, got: {s}"

end
end Postgres
