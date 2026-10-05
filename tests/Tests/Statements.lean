/-
Copyright (c) 2026 Paul Butcher. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Postgres
import PostgresTest.Framework
import Tests.Helpers

open Postgres
open Postgres.Test

/--
Statement lifecycle: parameterized `INSERT`/`SELECT` (incl. bound `NULL` and multi-row iteration),
`step` returning `false` immediately for a query matching no rows, and `UPDATE`/`DELETE` executing
fine via `exec` despite never having rows to step over. These are one test rather than three
because the pieces build on each other's inserted rows within a single transaction, and each test
runs inside its own `withRollback`, so splitting them apart would leave each piece unable to see
the previous one's writes.
-/
def testStatementLifecycle (conn : Conn) : TestM Unit :=
  withHeader "=== Testing statement lifecycle (bind/step/exec) ===" <| withRollback conn <| guardTest do
    let create ← prepare conn "CREATE TABLE IF NOT EXISTS leanpostgres_test_stmt (id integer, name text)"
    create.exec

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
    recordSuccess s!"parameterized round trip OK: {rows}"

    let selectMissing ← prepare conn "SELECT id FROM leanpostgres_test_stmt WHERE id = $1"
    selectMissing.bindText 1 "999"
    if ← selectMissing.step then
      throw <| IO.userError "expected no rows for id = 999"
    recordSuccess "empty result set correctly returns false immediately"

    let update ← prepare conn "UPDATE leanpostgres_test_stmt SET name = $1 WHERE id = $2"
    update.bindText 1 "Alicia"
    update.bindText 2 "1"
    update.exec

    let delete ← prepare conn "DELETE FROM leanpostgres_test_stmt WHERE id = $1"
    delete.bindText 1 "2"
    delete.exec

    recordSuccess "UPDATE/DELETE executed without error"

/-- `execScript` runs several `;`-separated statements in one call, including ones that produce
result rows (which are discarded), and later statements see earlier ones' effects. -/
def testExecScriptMultiStatement (conn : Conn) : TestM Unit :=
  withHeader "=== Testing execScript (multi-statement) ===" <| withRollback conn <| guardTest do
    execScript conn
      "CREATE TABLE leanpostgres_test_script (id integer);
       INSERT INTO leanpostgres_test_script VALUES (1);
       INSERT INTO leanpostgres_test_script VALUES (2);
       SELECT * FROM leanpostgres_test_script;"
    let select ← prepare conn "SELECT count(*) FROM leanpostgres_test_script"
    discard select.step
    let count ← select.columnText 0
    if count != "2" then
      throw <| IO.userError s!"expected 2 rows after script, got {count}"
    recordSuccess "multi-statement script executed, all statements applied"

/-- A failure partway through an `execScript` call rolls the whole script back: the server runs
it in one implicit transaction, so statements before the failing one leave no trace. -/
def testExecScriptImplicitTransaction (conn : Conn) : TestM Unit :=
  withHeader "=== Testing execScript (implicit transaction) ===" <| guardTest do
    try
      let caught ← try
          execScript conn
            "CREATE TABLE leanpostgres_test_script_atomic (id integer);
             INSERT INTO leanpostgres_test_script_atomic VALUES (not_a_column);"
          pure (none : Option IO.Error)
        catch e => pure (some e)
      match caught with
      | none => throw <| IO.userError "expected the script to fail, but it succeeded"
      | some e =>
        if Error.ofIOError? e |>.isNone then
          throw <| IO.userError s!"expected a Postgres.Error, got: {e}"
      let probe ← prepare conn
        "SELECT count(*) FROM pg_tables WHERE tablename = 'leanpostgres_test_script_atomic'"
      discard probe.step
      let count ← probe.columnText 0
      if count != "0" then
        throw <| IO.userError "expected the CREATE TABLE before the failure to be rolled back"
      recordSuccess "failing script left nothing applied"
    finally
      execScript conn "DROP TABLE IF EXISTS leanpostgres_test_script_atomic"

/-- A syntactically invalid statement surfaces a `Postgres.Error` with SQLSTATE `42601`. -/
def testMalformedStatementError (conn : Conn) : TestM Unit :=
  withHeader "=== Testing malformed statement error ===" <| withRollback conn <| guardTest do
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
        recordSuccess s!"malformed statement correctly surfaced SQLSTATE 42601: {pgErr}"

/--
A unique-constraint violation surfaces as SQLSTATE `23505`, not just a message string; asserting
on the code (not the message) is the behavior callers are meant to rely on.
-/
def testUniqueViolationSqlstate (conn : Conn) : TestM Unit :=
  withHeader "=== Testing unique violation SQLSTATE ===" <| withRollback conn <| guardTest do
    let create ← prepare conn
      "CREATE TABLE IF NOT EXISTS leanpostgres_test_unique (id integer PRIMARY KEY)"
    create.exec
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
        if pgErr.constraint != some "leanpostgres_test_unique_pkey" then
          throw <| IO.userError s!"expected the violated constraint to be named, got: {repr pgErr.constraint}"
        recordSuccess s!"unique violation correctly surfaced SQLSTATE 23505: {pgErr}"

#guard
  let e : Error := { sqlstate := "23505", message := "duplicate key", constraint := some "a]b; c%d" }
  Error.ofIOError? (.userError (toString e)) == some e

#guard Error.ofIOError? (.userError "[22012] division by zero") ==
  some { sqlstate := "22012", message := "division by zero" }

/--
Every index from 1 to `paramCount` round-trips, including the last one.

The parameter buffer is sized from `paramCount` at `prepare` time and indexed by `bind*` after a
range check against `paramCount` again; the two only agree because nothing resizes the buffer.
Should they ever diverge, an out-of-range bind is dropped rather than rejected, and the parameter
silently stays `NULL`, which is what this checks for.
-/
def testEveryParameterIndexBinds (conn : Conn) : TestM Unit :=
  withHeader "=== Testing every parameter index binds ===" <| withRollback conn <| guardTest do
    let values := #["a", "b", "c", "d", "e", "f", "g", "h", "i", "j"]
    let placeholders := String.intercalate ", " (values.zipIdx.toList.map fun (_, i) => s!"${i + 1}::text")
    let select ← prepare conn s!"SELECT {placeholders}"
    if select.paramCount != values.size then
      throw <| IO.userError s!"expected {values.size} parameters, got {select.paramCount}"
    for (value, i) in values.zipIdx do
      select.bindText (Int32.ofNat (i + 1)) value
    unless ← select.step do
      throw <| IO.userError "expected a row"
    let mut got : Array String := #[]
    for (_, i) in values.zipIdx do
      if ← select.columnIsNull (Int32.ofNat i) then
        throw <| IO.userError s!"parameter {i + 1} came back NULL: the bind was dropped"
      got := got.push (← select.columnText (Int32.ofNat i))
    if got != values then
      throw <| IO.userError s!"expected {values}, got {got}"
    recordSuccess s!"all {values.size} parameter indices bound and round-tripped"
