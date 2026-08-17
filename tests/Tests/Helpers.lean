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

/--
A connection string addressing a reachable host on a port nothing listens on, so every open fails
immediately with "connection refused" rather than depending on a timeout to give up.
-/
def unreachableConninfo : String :=
  "host=host.docker.internal port=1 dbname=leanpostgres user=leanpostgres connect_timeout=5"

/--
Waits for every task to finish, polling up to `attempts` times. Returns whether they all did.

Blocking on a task that never completes would hang the suite rather than fail it, which is exactly
the symptom of a pool that has lost capacity, so anything testing for that has to bound its wait.
-/
def waitForTasks (tasks : Array (Task α)) : Nat → IO Bool
  | 0 => return false
  | attempts + 1 => do
    let mut pending := false
    for task in tasks do
      unless ← IO.hasFinished task do
        pending := true
    unless pending do return true
    IO.sleep 10
    waitForTasks tasks attempts

/-- The backend process id `conn` is connected to. -/
def backendPid (conn : Conn) : IO String := do
  let stmt ← prepare conn "SELECT pg_backend_pid()"
  let _ ← stmt.step
  stmt.columnText 0

/--
The last statement the server saw on `pid`'s backend, which is how a test can tell whether anything
was sent on a connection without sending anything on it to find out.
-/
def lastQuery (observer : Conn) (pid : String) : IO String := do
  let stmt ← prepare observer "SELECT query FROM pg_stat_activity WHERE pid = $1::int"
  stmt.bindText 1 pid
  unless ← stmt.step do
    throw <| IO.userError s!"no backend {pid} in pg_stat_activity"
  stmt.columnText 0

/-- Terminates `pid`'s backend from `observer`, returning once the server reports it gone. -/
def terminateBackend (observer : Conn) (pid : String) : IO Unit := do
  let kill ← prepare observer "SELECT pg_terminate_backend($1::int)"
  kill.bindText 1 pid
  kill.exec
  unless ← waitForBackendGone observer pid 200 do
    throw <| IO.userError s!"backend {pid} was still running after pg_terminate_backend"
