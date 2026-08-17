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

/--
Polls `observer` until the server no longer reports a backend for `pid`, giving up after
`attempts` tries. Asking the server settles this definitively, where watching for the close to
reach the terminated connection's own client would only settle it eventually.
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
Polls `conn` until it reports itself no longer live, giving up after `attempts` tries. The close
has to travel back from the server before a local check can see it, so a single check immediately
after the backend goes would report the connection healthy.
-/
def waitUntilNotLive (conn : Conn) : Nat → IO Bool
  | 0 => return false
  | attempts + 1 => do
    if !(← conn.isLive) then return true
    IO.sleep 10
    waitUntilNotLive conn attempts

/-- The backend process id `conn` is connected to. -/
def backendPid (conn : Conn) : IO String := do
  let stmt ← prepare conn "SELECT pg_backend_pid()"
  let _ ← stmt.step
  stmt.columnText 0

/-- Terminates `pid`'s backend from `observer`, returning once the server reports it gone. -/
def terminateBackend (observer : Conn) (pid : String) : IO Unit := do
  let kill ← prepare observer "SELECT pg_terminate_backend($1::int)"
  kill.bindText 1 pid
  kill.exec
  unless ← waitForBackendGone observer pid 200 do
    throw <| IO.userError s!"backend {pid} was still running after pg_terminate_backend"
