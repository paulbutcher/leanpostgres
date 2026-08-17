/-
Copyright (c) 2026 Paul Butcher. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Postgres
import PostgresTest.Framework

open Postgres
open Postgres.Test

/--
`beginTransaction` generates valid `BEGIN` text for every `TransactionOptions` combination.

Not wrapped in `withRollback`; it's exercising `BEGIN`/`ROLLBACK` directly, and Postgres has no
true nested transactions, so an enclosing wrapper transaction would just get ended by the first
iteration's `rollback conn` instead of a fresh one starting per iteration.
-/
def testTransactionOptionsCombinations (conn : Conn) : TestM Unit :=
  withHeader "=== Testing BEGIN text generation for every TransactionOptions combination ===" <| guardTest do
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
    recordSuccess "BEGIN text generation OK for every TransactionOptions combination"

/--
`transaction` commits on success and rolls back on a thrown exception.

Not wrapped in `withRollback` for the same reason as `testTransactionOptionsCombinations`; this
test's whole point is exercising real commits/rollbacks, which an enclosing wrapper transaction
would interfere with. Cleans up its own table with `DELETE FROM` at the start instead.
-/
def testTransactionCommitAndRollback (conn : Conn) : TestM Unit :=
  withHeader "=== Testing transaction commit/rollback semantics ===" <| guardTest do
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

    recordSuccess "transaction commit/rollback OK"

/--
Polls `observer` until the server no longer reports a backend for `pid`, giving up after
`attempts` tries. Asking the server settles this definitively, where watching for the close to
reach this client would only settle it eventually.
-/
def waitForBackendGone (observer : Conn) (pid : String) : Nat → IO Bool
  | 0 => return false
  | attempts + 1 => do
    let stmt ← prepare observer "SELECT count(*) FROM pg_stat_activity WHERE pid = $1::int"
    stmt.bindText 1 pid
    let _ ← stmt.step
    if (← stmt.columnText 0) == "0" then return true
    IO.sleep 10
    waitForBackendGone observer pid attempts

/--
An error raised inside a transaction reaches the caller with its SQLSTATE even when the connection
dies before the rollback can run. A rollback failing is the characteristic symptom of exactly the
connection loss that makes the original error worth reporting accurately, so this is the case in
which the code is most easily lost and most needed.

Works on its own connection, which it destroys.
-/
def testTransactionSqlstateSurvivesFailedRollback : TestM Unit :=
  withHeader "=== Testing SQLSTATE survives a rollback that fails ===" <| guardTest do
    let victim ← «open» ""
    let observer ← «open» ""

    let pidStmt ← prepare victim "SELECT pg_backend_pid()"
    let _ ← pidStmt.step
    let pid ← pidStmt.columnText 0

    let caught ← try
        transaction victim (do
          let failed ← try
              (← prepare victim "SELECT 1 / 0").exec
              pure none
            catch e => pure (some e)
          let some original := failed
            | throw <| IO.userError "expected division by zero to be rejected by the server"
          let kill ← prepare observer "SELECT pg_terminate_backend($1::int)"
          kill.bindText 1 pid
          kill.exec
          unless ← waitForBackendGone observer pid 200 do
            throw <| IO.userError "backend was still running after pg_terminate_backend"
          throw original : IO Unit)
        pure (none : Option IO.Error)
      catch e => pure (some e)

    let some err := caught
      | throw <| IO.userError "expected the transaction to rethrow the action's error"
    let some parsed := Error.ofIOError? err
      | throw <| IO.userError s!"error reached the caller with no recoverable SQLSTATE: {err}"
    if parsed.sqlstate != "22012" then
      throw <| IO.userError
        s!"expected SQLSTATE 22012 to survive the failed rollback, got '{parsed.sqlstate}'"
    recordSuccess s!"SQLSTATE {parsed.sqlstate} survived a rollback that failed"
