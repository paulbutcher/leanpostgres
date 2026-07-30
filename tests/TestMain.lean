import Postgres

open Postgres

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

def main : IO Unit := do
  checkFFIInitialized
  checkConnectSuccess
  checkConnectFailure
  let conn ← «open» ""
  checkParameterizedRoundTrip conn
  checkEmptyResultSet conn
  checkNonSelectCommands conn
  checkMalformedStatementError conn
  IO.println "all checks passed"
