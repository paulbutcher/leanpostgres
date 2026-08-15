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
`columnName`/`columnTableName`/`columnOriginName`/`columnDatabaseName`, both unaliased (origin ==
alias) and aliased (origin differs from the alias `columnName` returns).
-/
def testColumnMetadata (conn : Conn) : TestM Unit :=
  withHeader "=== Testing column metadata ===" <| withRollback conn <| guardTest do
    let create ← prepare conn
      "CREATE TABLE IF NOT EXISTS leanpostgres_test_metadata
         (user_id integer, user_name text, user_email text)"
    create.exec

    let select ← prepare conn "SELECT user_id, user_name, user_email FROM leanpostgres_test_metadata"
    discard select.step

    let names ← #[0, 1, 2].mapM (Stmt.columnName select ·)
    if names != #["user_id", "user_name", "user_email"] then
      throw <| IO.userError s!"unexpected column names: {names}"

    let tableNames ← #[0, 1, 2].mapM (Stmt.columnTableName select ·)
    if tableNames != #["leanpostgres_test_metadata", "leanpostgres_test_metadata", "leanpostgres_test_metadata"] then
      throw <| IO.userError s!"unexpected column table names: {tableNames}"

    let originNames ← #[0, 1, 2].mapM (Stmt.columnOriginName select ·)
    if originNames != #["user_id", "user_name", "user_email"] then
      throw <| IO.userError s!"unexpected (unaliased) column origin names: {originNames}"

    let dbName ← select.columnDatabaseName 0
    if dbName.isEmpty then throw <| IO.userError "expected a non-empty database name"

    let selectAliased ← prepare conn
      "SELECT user_id AS id, user_name AS name FROM leanpostgres_test_metadata"
    discard selectAliased.step

    let aliasNames ← #[0, 1].mapM (Stmt.columnName selectAliased ·)
    if aliasNames != #["id", "name"] then throw <| IO.userError s!"unexpected aliased column names: {aliasNames}"

    let aliasOrigins ← #[0, 1].mapM (Stmt.columnOriginName selectAliased ·)
    if aliasOrigins != #["user_id", "user_name"] then
      throw <| IO.userError s!"unexpected aliased column origin names: {aliasOrigins}"

    recordSuccess s!"Column metadata OK (name/tableName/originName/databaseName={dbName}, incl. aliasing)"

/--
`columnCount` for an explicit select list, for `SELECT *` (where the count isn't knowable
client-side), and for a command with no result columns at all.
-/
def testColumnCount (conn : Conn) : TestM Unit :=
  withHeader "=== Testing column count ===" <| withRollback conn <| guardTest do
    let create ← prepare conn
      "CREATE TABLE IF NOT EXISTS leanpostgres_test_column_count
         (id integer, name text, email text, created_at date)"
    create.exec

    let explicit ← prepare conn "SELECT id, name FROM leanpostgres_test_column_count"
    discard explicit.step
    let explicitCount ← explicit.columnCount
    if explicitCount != 2 then
      throw <| IO.userError s!"unexpected columnCount for an explicit select list: {explicitCount}"

    let star ← prepare conn "SELECT * FROM leanpostgres_test_column_count"
    discard star.step
    let starCount ← star.columnCount
    if starCount != 4 then throw <| IO.userError s!"unexpected columnCount for SELECT *: {starCount}"

    -- `step` returns `false` here (no rows), but the result, and so its metadata, is still there.
    let insert ← prepare conn "INSERT INTO leanpostgres_test_column_count (id) VALUES ($1)"
    insert.bind 1 (1 : Int32)
    let stepped ← insert.step
    if stepped then throw <| IO.userError "INSERT without RETURNING unexpectedly reported a row"
    let insertCount ← insert.columnCount
    if insertCount != 0 then
      throw <| IO.userError s!"unexpected columnCount for an INSERT without RETURNING: {insertCount}"

    let unstepped ← prepare conn "SELECT id FROM leanpostgres_test_column_count"
    expectFailure "columnCount before step throws" unstepped.columnCount

    recordSuccess "Column count OK (explicit select list/SELECT */no result columns)"

/--
`commandTag`/`commandTuples`/`isReadOnly` across `SELECT`/`INSERT`/`UPDATE`/`DELETE`/`BEGIN`.

Not wrapped in `withRollback`; it deliberately issues its own `BEGIN`/`ROLLBACK` (to check
`isReadOnly`'s classification of them), which would conflict with an enclosing wrapper transaction
the same way it would in `testTransactionOptionsCombinations`/`testTransactionCommitAndRollback`.
Cleans up its own table with `DELETE FROM` at the start instead.
-/
def testCommandMetadata (conn : Conn) : TestM Unit :=
  withHeader "=== Testing command metadata ===" <| guardTest do
    let create ← prepare conn "CREATE TABLE IF NOT EXISTS leanpostgres_test_command (id integer, val text)"
    create.exec
    let clear ← prepare conn "DELETE FROM leanpostgres_test_command"
    clear.exec

    let insert ← prepare conn "INSERT INTO leanpostgres_test_command (id, val) VALUES ($1, $2), ($3, $4)"
    insert.bind 1 (1 : Int32)
    insert.bind 2 "a"
    insert.bind 3 (2 : Int32)
    insert.bind 4 "b"
    discard insert.step
    let insertTag ← insert.commandTag
    let insertTuples ← insert.commandTuples
    let insertReadOnly ← insert.isReadOnly
    if !insertTag.startsWith "INSERT" then throw <| IO.userError s!"unexpected INSERT command tag: {insertTag}"
    if insertTuples != some 2 then throw <| IO.userError s!"unexpected INSERT commandTuples: {insertTuples}"
    if insertReadOnly then throw <| IO.userError "INSERT incorrectly classified as read-only"

    let update ← prepare conn "UPDATE leanpostgres_test_command SET val = $1 WHERE id = $2"
    update.bind 1 "updated"
    update.bind 2 (1 : Int32)
    discard update.step
    let updateTag ← update.commandTag
    let updateTuples ← update.commandTuples
    if updateTag != "UPDATE 1" then throw <| IO.userError s!"unexpected UPDATE command tag: {updateTag}"
    if updateTuples != some 1 then throw <| IO.userError s!"unexpected UPDATE commandTuples: {updateTuples}"

    let delete ← prepare conn "DELETE FROM leanpostgres_test_command WHERE id = $1"
    delete.bind 1 (2 : Int32)
    discard delete.step
    let deleteTuples ← delete.commandTuples
    if deleteTuples != some 1 then throw <| IO.userError s!"unexpected DELETE commandTuples: {deleteTuples}"

    let select ← prepare conn "SELECT * FROM leanpostgres_test_command"
    discard select.step
    let selectTag ← select.commandTag
    let selectTuples ← select.commandTuples
    let selectReadOnly ← select.isReadOnly
    -- one row survives the insert-2/update-1/delete-1 sequence above.
    if !selectTag.startsWith "SELECT" then throw <| IO.userError s!"unexpected SELECT command tag: {selectTag}"
    if selectTuples != some 1 then throw <| IO.userError s!"unexpected SELECT commandTuples: {selectTuples}"
    if !selectReadOnly then throw <| IO.userError "SELECT incorrectly classified as not read-only"

    let begin_ ← prepare conn "BEGIN"
    discard begin_.step
    let beginReadOnly ← begin_.isReadOnly
    if !beginReadOnly then throw <| IO.userError "BEGIN incorrectly classified as not read-only"
    (← prepare conn "ROLLBACK").exec

    recordSuccess "Command metadata OK (commandTag/commandTuples/isReadOnly across SELECT/INSERT/UPDATE/DELETE/BEGIN)"
