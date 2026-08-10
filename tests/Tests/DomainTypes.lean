/-
Copyright (c) 2026 Paul Butcher. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Postgres
import PostgresTest.Framework
import Tests.Helpers

open Postgres
open Postgres.Test

/-- Round-trips `Postgres.Numeric`, including `NaN`/`Infinity`/`-Infinity` and trailing-zero scale. -/
def testNumericRoundTrip (conn : Conn) : TestM Unit :=
  withHeader "=== Testing Numeric round trip ===" <| withRollback conn <| guardTest do
    let create ← prepare conn "CREATE TABLE IF NOT EXISTS leanpostgres_test_numeric (id integer, n numeric)"
    create.exec

    let values : Array (Int32 × Numeric) := #[
      (1, .ofDigits false 1234500 4),
      (2, .ofDigits true 1 3),
      (3, .ofDigits false 0 0),
      (4, .ofDigits false 100 0),
      (5, .nan),
      (6, .posInf),
      (7, .negInf)
    ]
    for (id, n) in values do
      let insert ← prepare conn "INSERT INTO leanpostgres_test_numeric (id, n) VALUES ($1, $2)"
      insert.bind 1 id
      insert.bind 2 n
      insert.exec

    for (id, expected) in values do
      let select ← prepare conn "SELECT n FROM leanpostgres_test_numeric WHERE id = $1"
      select.bind 1 id
      let hasRow ← select.step
      if !hasRow then throw <| IO.userError s!"expected a row for id {id}"
      let n ← ResultColumn.get (α := Numeric) select 0
      if n != expected then
        throw <| IO.userError s!"Numeric round trip failed for id {id}: expected {repr expected}, got {repr n}"

    recordSuccess "Numeric round trip OK (incl. NaN/Infinity/-Infinity, trailing zeros)"

/-- Round-trips `Postgres.Uuid` through a real `uuid` column. -/
def testUuidRoundTrip (conn : Conn) : TestM Unit :=
  withHeader "=== Testing Uuid round trip ===" <| withRollback conn <| guardTest do
    let create ← prepare conn "CREATE TABLE IF NOT EXISTS leanpostgres_test_uuid (id integer, u uuid)"
    create.exec

    match Uuid.ofText? "123e4567-e89b-12d3-a456-426614174000" with
    | none => throw <| IO.userError "failed to parse test uuid literal"
    | some uuid =>
      let insert ← prepare conn "INSERT INTO leanpostgres_test_uuid (id, u) VALUES ($1, $2)"
      insert.bind 1 (1 : Int32)
      insert.bind 2 uuid
      insert.exec

      let select ← prepare conn "SELECT u FROM leanpostgres_test_uuid WHERE id = $1"
      select.bind 1 (1 : Int32)
      let hasRow ← select.step
      if !hasRow then throw <| IO.userError "expected a row"
      let result ← ResultColumn.get (α := Uuid) select 0
      if result != uuid then
        throw <| IO.userError s!"Uuid round trip failed: expected {uuid.toText}, got {result.toText}"
      recordSuccess s!"Uuid round trip OK ({result.toText})"

/-- Round-trips `Std.Time.PlainDate` through a real `date` column, incl. a leap day. -/
def testDateRoundTrip (conn : Conn) : TestM Unit :=
  withHeader "=== Testing Date round trip ===" <| withRollback conn <| guardTest do
    let create ← prepare conn "CREATE TABLE IF NOT EXISTS leanpostgres_test_date (id integer, d date)"
    create.exec

    match dateOfText? "2024-02-29" with
    | none => throw <| IO.userError "failed to parse test date literal"
    | some d =>
      let insert ← prepare conn "INSERT INTO leanpostgres_test_date (id, d) VALUES ($1, $2)"
      insert.bind 1 (1 : Int32)
      insert.bind 2 d
      insert.exec

      let select ← prepare conn "SELECT d FROM leanpostgres_test_date WHERE id = $1"
      select.bind 1 (1 : Int32)
      let hasRow ← select.step
      if !hasRow then throw <| IO.userError "expected a row"
      let result ← ResultColumn.get (α := Std.Time.PlainDate) select 0
      if result != d then
        throw <| IO.userError s!"date round trip failed: expected {dateToText d}, got {dateToText result}"

    if (dateOfText? "2026-02-30").isSome then
      throw <| IO.userError "expected Feb 30 to be rejected as an invalid date"

    recordSuccess "Date round trip OK (incl. leap day, invalid-date rejection)"

/-- Round-trips `Postgres.Time` through both `time` (no zone) and `time with time zone` columns. -/
def testTimeRoundTrip (conn : Conn) : TestM Unit :=
  withHeader "=== Testing Time/timetz round trip ===" <| withRollback conn <| guardTest do
    let create ← prepare conn
      "CREATE TABLE IF NOT EXISTS leanpostgres_test_time (id integer, t time, tz timetz)"
    create.exec

    match Time.ofText? "13:05:07.5", Time.ofText? "13:05:07+05:30" with
    | none, _ | _, none => throw <| IO.userError "failed to parse test time literals"
    | some plain, some withOffset =>
      let insert ← prepare conn "INSERT INTO leanpostgres_test_time (id, t, tz) VALUES ($1, $2, $3)"
      insert.bind 1 (1 : Int32)
      insert.bind 2 plain
      insert.bind 3 withOffset
      insert.exec

      let select ← prepare conn "SELECT t, tz FROM leanpostgres_test_time WHERE id = $1"
      select.bind 1 (1 : Int32)
      let hasRow ← select.step
      if !hasRow then throw <| IO.userError "expected a row"
      let resultPlain ← ResultColumn.get (α := Time) select 0
      let resultTz ← ResultColumn.get (α := Time) select 1
      if resultPlain != plain then
        throw <| IO.userError s!"time round trip failed: expected {plain.toText}, got {resultPlain.toText}"
      -- unlike timestamptz, Postgres preserves timetz's original offset verbatim (no session-zone
      -- normalization), so this should be an exact round trip, not just "has some offset".
      if resultTz != withOffset then
        throw <| IO.userError s!"timetz round trip failed: expected {withOffset.toText}, got {resultTz.toText}"
      recordSuccess s!"Time/timetz round trip OK ({resultPlain.toText}, {resultTz.toText})"

/-- Round-trips `Std.Time.PlainDateTime` through a real `timestamp` column. -/
def testTimestampRoundTrip (conn : Conn) : TestM Unit :=
  withHeader "=== Testing Timestamp round trip ===" <| withRollback conn <| guardTest do
    let create ← prepare conn "CREATE TABLE IF NOT EXISTS leanpostgres_test_timestamp (id integer, ts timestamp)"
    create.exec

    match timestampOfText? "2026-07-30 13:05:07.5" with
    | none => throw <| IO.userError "failed to parse test timestamp literal"
    | some ts =>
      let insert ← prepare conn "INSERT INTO leanpostgres_test_timestamp (id, ts) VALUES ($1, $2)"
      insert.bind 1 (1 : Int32)
      insert.bind 2 ts
      insert.exec

      let select ← prepare conn "SELECT ts FROM leanpostgres_test_timestamp WHERE id = $1"
      select.bind 1 (1 : Int32)
      let hasRow ← select.step
      if !hasRow then throw <| IO.userError "expected a row"
      let result ← ResultColumn.get (α := Std.Time.PlainDateTime) select 0
      if result != ts then
        throw <| IO.userError
          s!"timestamp round trip failed: expected {timestampToText ts}, got {timestampToText result}"
      recordSuccess s!"Timestamp round trip OK ({timestampToText result})"

/--
Round-trips `Std.Time.DateTime` through a real `timestamptz` column, with the session `TimeZone`
explicitly pinned to UTC first; Postgres normalizes `timestamptz` display to the session zone, so
an unpinned test would be a flaky, environment-dependent check. Pinning it via plain `SET` inside
`withRollback`'s transaction confines the change to this test alone (a transactional `SET` reverts
on rollback), rather than leaking into the session for every test that runs after this one.
-/
def testTimestamptzRoundTrip (conn : Conn) : TestM Unit :=
  withHeader "=== Testing Timestamptz round trip ===" <| withRollback conn <| guardTest do
    let setTz ← prepare conn "SET TimeZone = 'UTC'"
    setTz.exec

    let create ← prepare conn
      "CREATE TABLE IF NOT EXISTS leanpostgres_test_timestamptz (id integer, ts timestamptz)"
    create.exec

    match timestamptzOfText? "2026-07-30 13:05:07.5+05:30" with
    | none => throw <| IO.userError "failed to parse test timestamptz literal"
    | some dt =>
      let insert ← prepare conn "INSERT INTO leanpostgres_test_timestamptz (id, ts) VALUES ($1, $2)"
      insert.bind 1 (1 : Int32)
      insert.bind 2 dt
      insert.exec

      let select ← prepare conn "SELECT ts FROM leanpostgres_test_timestamptz WHERE id = $1"
      select.bind 1 (1 : Int32)
      let hasRow ← select.step
      if !hasRow then throw <| IO.userError "expected a row"
      let result ← ResultColumn.get (α := Std.Time.DateTime) select 0
      -- Compare the absolute instant, not the raw offset, since the session TimeZone (pinned to
      -- UTC above) normalizes the displayed offset to +00 regardless of what was inserted.
      if result.toTimestamp != dt.toTimestamp then
        throw <| IO.userError
          s!"timestamptz round trip failed: expected instant {timestamptzToText dt}, got {timestamptzToText result}"
      if result.timezone.offset.second.val != 0 then
        throw <| IO.userError
          s!"expected timestamptz to normalize to the pinned UTC session TimeZone, got offset {result.timezone.offset.second.val}"
      recordSuccess s!"Timestamptz round trip OK ({timestamptzToText result}, session TimeZone pinned to UTC)"

/--
Round-trips 1-D arrays, including an empty array, a `NULL` element, and string elements needing
`{a,b,c}`-grammar quoting (comma, space, embedded quote, backslash, and the literal word `NULL`).
-/
def testArrayRoundTrip (conn : Conn) : TestM Unit :=
  withHeader "=== Testing Array round trip ===" <| withRollback conn <| guardTest do
    let create ← prepare conn
      "CREATE TABLE IF NOT EXISTS leanpostgres_test_array
         (id integer, ints integer[], names text[], maybe_ints integer[])"
    create.exec

    let ints : Array Int32 := #[1, -2, 3]
    let names : Array String := #["hello", "a,b", "with space", "she said \"hi\"", "NULL", "", "back\\slash"]
    let maybeInts : Array (Option Int32) := #[some 1, none, some (-3)]

    let insert ← prepare conn
      "INSERT INTO leanpostgres_test_array (id, ints, names, maybe_ints) VALUES ($1, $2, $3, $4)"
    insert.bind 1 (1 : Int32)
    insert.bind 2 ints
    insert.bind 3 names
    insert.bind 4 maybeInts
    insert.exec

    let insertEmpty ← prepare conn
      "INSERT INTO leanpostgres_test_array (id, ints, names, maybe_ints) VALUES ($1, $2, $3, $4)"
    insertEmpty.bind 1 (2 : Int32)
    insertEmpty.bind 2 (#[] : Array Int32)
    insertEmpty.bind 3 (#[] : Array String)
    insertEmpty.bind 4 (#[] : Array (Option Int32))
    insertEmpty.exec

    let select ← prepare conn
      "SELECT ints, names, maybe_ints FROM leanpostgres_test_array WHERE id = $1"
    select.bind 1 (1 : Int32)
    let hasRow ← select.step
    if !hasRow then throw <| IO.userError "expected a row"
    let resultInts ← ResultColumn.get (α := Array Int32) select 0
    let resultNames ← ResultColumn.get (α := Array String) select 1
    let resultMaybeInts ← ResultColumn.get (α := Array (Option Int32)) select 2
    if resultInts != ints then throw <| IO.userError s!"Int32 array round trip failed: {repr resultInts}"
    if resultNames != names then throw <| IO.userError s!"String array round trip failed: {repr resultNames}"
    if resultMaybeInts != maybeInts then
      throw <| IO.userError s!"Option Int32 array round trip failed: {repr resultMaybeInts}"

    let selectEmpty ← prepare conn
      "SELECT ints, names, maybe_ints FROM leanpostgres_test_array WHERE id = $1"
    selectEmpty.bind 1 (2 : Int32)
    let hasRow2 ← selectEmpty.step
    if !hasRow2 then throw <| IO.userError "expected a second row"
    let emptyInts ← ResultColumn.get (α := Array Int32) selectEmpty 0
    let emptyNames ← ResultColumn.get (α := Array String) selectEmpty 1
    let emptyMaybeInts ← ResultColumn.get (α := Array (Option Int32)) selectEmpty 2
    if !emptyInts.isEmpty || !emptyNames.isEmpty || !emptyMaybeInts.isEmpty then
      throw <| IO.userError "expected all three array columns to round-trip as empty"

    recordSuccess "Array round trip OK (Int32/String/Option Int32, incl. empty array, NULL element, quoting)"
