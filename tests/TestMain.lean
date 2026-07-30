import Postgres

open Postgres
open Postgres.Interpolation

/-!
Smoke test: importing `Postgres` pulls in `Postgres.FFI`, whose
`initModule` runs automatically at startup. If the native binding
object failed to link, or `leanpostgres_initialize` isn't wired up
correctly, this executable would fail to start rather than print.
-/

def checkFFIInitialized : IO Unit :=
  IO.println "leanpostgres: FFI initialized OK"

/-- Connects against the live Postgres instance addressed by the standard `PG*` env vars. -/
def checkConnectSuccess : IO Unit := do
  let conn ← «open» ""
  IO.println s!"connected: {repr conn}"

/--
Connects to a reachable host on a port nothing listens on, so libpq fails fast with
"connection refused" rather than needing a `connect_timeout` to avoid hanging.
-/
def checkConnectFailure : IO Unit := do
  let badConninfo := "host=host.docker.internal port=1 dbname=leanpostgres user=leanpostgres connect_timeout=5"
  let caught ← try
      let _ ← «open» badConninfo
      pure (none : Option IO.Error)
    catch e => pure (some e)
  match caught with
  | none => throw <| IO.userError "expected connecting to a closed port to fail, but it succeeded"
  | some e =>
    match Error.ofIOError? e with
    | none => throw <| IO.userError s!"expected a Postgres.Error, got: {e}"
    | some pgErr =>
      if pgErr.message.isEmpty then
        throw <| IO.userError "expected a non-empty error message on connect failure"
      IO.println s!"connect failure correctly surfaced as a Postgres.Error: {pgErr}"

/-- Parameterized `INSERT`/`SELECT` round trip, including a bound `NULL` and multi-row iteration. -/
def checkParameterizedRoundTrip (conn : Conn) : IO Unit := do
  let create ← prepare conn "CREATE TABLE IF NOT EXISTS leanpostgres_test_stmt (id integer, name text)"
  create.exec
  let clear ← prepare conn "DELETE FROM leanpostgres_test_stmt"
  clear.exec

  let insert1 ← prepare conn "INSERT INTO leanpostgres_test_stmt (id, name) VALUES ($1, $2)"
  insert1.bindText 1 "1"
  insert1.bindText 2 "Alice"
  insert1.exec

  let insert2 ← prepare conn "INSERT INTO leanpostgres_test_stmt (id, name) VALUES ($1, $2)"
  insert2.bindText 1 "2"
  insert2.bindNull 2
  insert2.exec

  let select ← prepare conn "SELECT id, name FROM leanpostgres_test_stmt ORDER BY id"
  let mut hasRow ← select.step
  let mut rows : Array (String × Option String) := #[]
  while hasRow do
    let id ← select.columnText 0
    let name ← if ← select.columnIsNull 1 then pure none else some <$> select.columnText 1
    rows := rows.push (id, name)
    hasRow ← select.step

  let expected := #[("1", some "Alice"), ("2", none)]
  if rows != expected then
    throw <| IO.userError s!"expected {expected}, got {rows}"
  IO.println s!"parameterized round trip OK: {rows}"

/-- `step` on a query matching no rows returns `false` immediately. -/
def checkEmptyResultSet (conn : Conn) : IO Unit := do
  let select ← prepare conn "SELECT id FROM leanpostgres_test_stmt WHERE id = $1"
  select.bindText 1 "999"
  if ← select.step then
    throw <| IO.userError "expected no rows for id = 999"
  IO.println "empty result set correctly returns false immediately"

/-- `UPDATE`/`DELETE` execute fine via `exec` despite never having rows to step over. -/
def checkNonSelectCommands (conn : Conn) : IO Unit := do
  let update ← prepare conn "UPDATE leanpostgres_test_stmt SET name = $1 WHERE id = $2"
  update.bindText 1 "Alicia"
  update.bindText 2 "1"
  update.exec

  let delete ← prepare conn "DELETE FROM leanpostgres_test_stmt WHERE id = $1"
  delete.bindText 1 "2"
  delete.exec

  IO.println "UPDATE/DELETE executed without error"

/-- A syntactically invalid statement surfaces a `Postgres.Error` with SQLSTATE `42601`. -/
def checkMalformedStatementError (conn : Conn) : IO Unit := do
  let badStmt ← prepare conn "SELEKT * FROM nonexistent_syntax_error"
  let caught ← try
      let _ ← badStmt.step
      pure (none : Option IO.Error)
    catch e => pure (some e)
  match caught with
  | none => throw <| IO.userError "expected a syntax error, but the statement succeeded"
  | some e =>
    match Error.ofIOError? e with
    | none => throw <| IO.userError s!"expected a Postgres.Error, got: {e}"
    | some pgErr =>
      if pgErr.sqlstate != "42601" then
        throw <| IO.userError s!"expected SQLSTATE 42601, got: {pgErr}"
      IO.println s!"malformed statement correctly surfaced SQLSTATE 42601: {pgErr}"

/--
Round-trips every M3 core type through `QueryParam`/`ResultColumn`, including bound and read
`NULL` (`Option α`).
-/
def checkCoreTypeRoundTrip (conn : Conn) : IO Unit := do
  let create ← prepare conn
    "CREATE TABLE IF NOT EXISTS leanpostgres_test_types
       (flag boolean, small smallint, n integer, big bigint,
        r real, d double precision, t text, b bytea)"
  create.exec
  let clear ← prepare conn "DELETE FROM leanpostgres_test_types"
  clear.exec

  let insert ← prepare conn
    "INSERT INTO leanpostgres_test_types (flag, small, n, big, r, d, t, b)
       VALUES ($1, $2, $3, $4, $5, $6, $7, $8)"
  insert.bind 1 true
  insert.bind 2 (42 : Int16)
  insert.bind 3 (-7 : Int32)
  insert.bind 4 (9223372036854775807 : Int64)
  insert.bind 5 (3.5 : Float32)
  insert.bind 6 (2.71828 : Float)
  insert.bind 7 "hello"
  insert.bind 8 (ByteArray.mk #[0xDE, 0xAD, 0xBE, 0xEF])
  insert.exec

  let insertNulls ← prepare conn
    "INSERT INTO leanpostgres_test_types (flag, small, n, big, r, d, t, b)
       VALUES ($1, $2, $3, $4, $5, $6, $7, $8)"
  insertNulls.bind 1 (none : Option Bool)
  insertNulls.bind 2 (none : Option Int16)
  insertNulls.bind 3 (none : Option Int32)
  insertNulls.bind 4 (none : Option Int64)
  insertNulls.bind 5 (none : Option Float32)
  insertNulls.bind 6 (none : Option Float)
  insertNulls.bind 7 (none : Option String)
  insertNulls.bind 8 (none : Option ByteArray)
  insertNulls.exec

  let select ← prepare conn
    "SELECT flag, small, n, big, r, d, t, b FROM leanpostgres_test_types ORDER BY small NULLS LAST"

  let mut hasRow ← select.step
  if !hasRow then throw <| IO.userError "expected a first row"
  let flag ← ResultColumn.get (α := Option Bool) select 0
  let small ← ResultColumn.get (α := Option Int16) select 1
  let n ← ResultColumn.get (α := Option Int32) select 2
  let big ← ResultColumn.get (α := Option Int64) select 3
  let r ← ResultColumn.get (α := Option Float32) select 4
  let d ← ResultColumn.get (α := Option Float) select 5
  let t ← ResultColumn.get (α := Option String) select 6
  let b ← ResultColumn.get (α := Option ByteArray) select 7
  if flag != some true then throw <| IO.userError s!"Bool round trip failed: {flag}"
  if small != some (42 : Int16) then throw <| IO.userError s!"Int16 round trip failed: {repr small}"
  if n != some (-7 : Int32) then throw <| IO.userError s!"Int32 round trip failed: {repr n}"
  if big != some (9223372036854775807 : Int64) then
    throw <| IO.userError s!"Int64 round trip failed: {repr big}"
  if r != some (3.5 : Float32) then throw <| IO.userError s!"Float32 round trip failed: {repr r}"
  if d != some (2.71828 : Float) then throw <| IO.userError s!"Float round trip failed: {repr d}"
  if t != some "hello" then throw <| IO.userError s!"String round trip failed: {t}"
  if b != some (ByteArray.mk #[0xDE, 0xAD, 0xBE, 0xEF]) then
    throw <| IO.userError s!"ByteArray round trip failed: {b.map ByteArray.toList}"

  hasRow ← select.step
  if !hasRow then throw <| IO.userError "expected a second (all-NULL) row"
  let flag2 ← ResultColumn.get (α := Option Bool) select 0
  let small2 ← ResultColumn.get (α := Option Int16) select 1
  let n2 ← ResultColumn.get (α := Option Int32) select 2
  let big2 ← ResultColumn.get (α := Option Int64) select 3
  let r2 ← ResultColumn.get (α := Option Float32) select 4
  let d2 ← ResultColumn.get (α := Option Float) select 5
  let t2 ← ResultColumn.get (α := Option String) select 6
  let b2 ← ResultColumn.get (α := Option ByteArray) select 7
  if flag2.isSome || small2.isSome || n2.isSome || big2.isSome || r2.isSome || d2.isSome ||
     t2.isSome || b2.isSome then
    throw <| IO.userError "expected every column of the second row to be NULL"

  IO.println "core type round trip OK (Bool/Int16/Int32/Int64/Float32/Float/String/ByteArray, incl. NULL)"

/--
Uses the tuple `Row` instance and `Stmt.resultsAs` to read a 3-column table and iterate a
multi-row result to completion.
-/
def checkTupleRowIteration (conn : Conn) : IO Unit := do
  let create ← prepare conn
    "CREATE TABLE IF NOT EXISTS leanpostgres_test_rows (id integer, name text, active boolean)"
  create.exec
  let clear ← prepare conn "DELETE FROM leanpostgres_test_rows"
  clear.exec

  for (id, name, active) in
      [((1 : Int32), "Alice", true), (2, "Bob", false), (3, "Carol", true)] do
    let insert ← prepare conn "INSERT INTO leanpostgres_test_rows (id, name, active) VALUES ($1, $2, $3)"
    insert.bind 1 id
    insert.bind 2 name
    insert.bind 3 active
    insert.exec

  let select ← prepare conn "SELECT id, name, active FROM leanpostgres_test_rows ORDER BY id"
  let mut rows : Array (Int32 × String × Bool) := #[]
  for row in select.resultsAs (Int32 × String × Bool) do
    rows := rows.push row

  let expected := #[((1 : Int32), "Alice", true), (2, "Bob", false), (3, "Carol", true)]
  if rows != expected then
    throw <| IO.userError s!"expected {expected}, got {rows}"
  IO.println s!"tuple Row + multi-row iteration OK: {rows}"

/-- Exercises `exec!`/`query!` with a mix of literal SQL and interpolated parameters. -/
def checkInterpolationMacros (conn : Conn) : IO Unit := do
  conn exec!"CREATE TABLE IF NOT EXISTS leanpostgres_test_interp (id integer, name text, active boolean)"
  conn exec!"DELETE FROM leanpostgres_test_interp"

  let people : List (Int32 × String × Bool) := [(1, "Alice", true), (2, "Bob", false), (3, "Carol", true)]
  for (id, name, active) in people do
    conn exec!"INSERT INTO leanpostgres_test_interp (id, name, active) VALUES ({id}, {name}, {active})"

  let minId : Int32 := 2
  let rows ←
    conn query!"SELECT id, name, active FROM leanpostgres_test_interp WHERE id >= {minId} ORDER BY id"
      as (Int32 × String × Bool)
  let mut collected : Array (Int32 × String × Bool) := #[]
  for row in rows do
    collected := collected.push row

  let expected := #[((2 : Int32), "Bob", false), (3, "Carol", true)]
  if collected != expected then
    throw <| IO.userError s!"expected {expected}, got {collected}"
  IO.println s!"interpolation macros OK: {collected}"

/-- `beginTransaction` generates valid `BEGIN` text for every `TransactionOptions` combination. -/
def checkTransactionOptionsCombinations (conn : Conn) : IO Unit := do
  let combos : List TransactionOptions := [
    {},
    { isolation := some .readCommitted },
    { isolation := some .repeatableRead },
    { isolation := some .serializable },
    { readOnly := true },
    { deferrable := true },
    { isolation := some .serializable, readOnly := true, deferrable := true }
  ]
  for opts in combos do
    beginTransaction conn opts
    rollback conn
  IO.println "BEGIN text generation OK for every TransactionOptions combination"

/-- `transaction` commits on success and rolls back on a thrown exception. -/
def checkTransactionCommitAndRollback (conn : Conn) : IO Unit := do
  let create ← prepare conn "CREATE TABLE IF NOT EXISTS leanpostgres_test_txn (id integer)"
  create.exec
  let clear ← prepare conn "DELETE FROM leanpostgres_test_txn"
  clear.exec

  let _ ← transaction conn (do
    let insert ← prepare conn "INSERT INTO leanpostgres_test_txn (id) VALUES (1)"
    insert.exec)
  let select ← prepare conn "SELECT id FROM leanpostgres_test_txn WHERE id = 1"
  if !(← select.step) then
    throw <| IO.userError "expected the committed row to be visible after transaction succeeded"

  let caught ← try
      let _ ← transaction conn (do
        let insert ← prepare conn "INSERT INTO leanpostgres_test_txn (id) VALUES (2)"
        insert.exec
        throw <| IO.userError "boom" : IO Unit)
      pure (none : Option IO.Error)
    catch e => pure (some e)
  if caught.isNone then
    throw <| IO.userError "expected the action's exception to propagate out of `transaction`"

  let select2 ← prepare conn "SELECT id FROM leanpostgres_test_txn WHERE id = 2"
  if ← select2.step then
    throw <| IO.userError "expected the rolled-back row to be absent after transaction threw"

  IO.println "transaction commit/rollback OK"

/--
A unique-constraint violation surfaces as SQLSTATE `23505`, not just a message string — asserting
on the code (not the message) is the behavior callers are meant to rely on.
-/
def checkUniqueViolationSqlstate (conn : Conn) : IO Unit := do
  let create ← prepare conn
    "CREATE TABLE IF NOT EXISTS leanpostgres_test_unique (id integer PRIMARY KEY)"
  create.exec
  let clear ← prepare conn "DELETE FROM leanpostgres_test_unique"
  clear.exec
  let insert1 ← prepare conn "INSERT INTO leanpostgres_test_unique (id) VALUES (1)"
  insert1.exec

  let insert2 ← prepare conn "INSERT INTO leanpostgres_test_unique (id) VALUES (1)"
  let caught ← try
      insert2.exec
      pure (none : Option IO.Error)
    catch e => pure (some e)
  match caught with
  | none => throw <| IO.userError "expected a unique constraint violation, but the insert succeeded"
  | some e =>
    match Error.ofIOError? e with
    | none => throw <| IO.userError s!"expected a Postgres.Error, got: {e}"
    | some pgErr =>
      if pgErr.sqlstate != "23505" then
        throw <| IO.userError s!"expected SQLSTATE 23505, got: {pgErr}"
      IO.println s!"unique violation correctly surfaced SQLSTATE 23505: {pgErr}"

def main : IO Unit := do
  checkFFIInitialized
  checkConnectSuccess
  checkConnectFailure
  let conn ← «open» ""
  checkParameterizedRoundTrip conn
  checkEmptyResultSet conn
  checkNonSelectCommands conn
  checkMalformedStatementError conn
  checkCoreTypeRoundTrip conn
  checkTupleRowIteration conn
  checkInterpolationMacros conn
  checkTransactionOptionsCombinations conn
  checkTransactionCommitAndRollback conn
  checkUniqueViolationSqlstate conn
  IO.println "all checks passed"
