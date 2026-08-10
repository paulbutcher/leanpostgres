/-
Copyright (c) 2026 Paul Butcher. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Postgres
import PostgresTest.Framework

open Postgres
open Postgres.Test

/--
Runs `action` inside a transaction that's always rolled back afterward, regardless of outcome;
keeps each test's writes from leaking into the next test, or into a re-run against the same
database. Tests that are themselves about transaction control (`BEGIN`/`COMMIT`/`ROLLBACK`
semantics) don't use this, since Postgres has no true nested transactions: a real `commit`/
`rollback` inside here would end this wrapper transaction too, not a nested one.
-/
def withRollback (conn : Conn) (action : TestM α) : TestM α := do
  beginTransaction conn
  try action finally rollback conn
