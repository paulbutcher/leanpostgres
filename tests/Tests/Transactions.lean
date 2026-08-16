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
