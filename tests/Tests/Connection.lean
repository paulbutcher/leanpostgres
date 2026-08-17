/-
Copyright (c) 2026 Paul Butcher. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Postgres
import PostgresTest.Framework
import Tests.Helpers

open Postgres
open Postgres.Test

/-!
Smoke test: importing `Postgres` pulls in `Postgres.FFI`, whose
`initModule` runs automatically at startup. If the native binding
object failed to link, or `leanpostgres_initialize` isn't wired up
correctly, this executable would fail to start rather than print.
-/

def testFFIInitialized : TestM Unit :=
  withHeader "=== Testing FFI initialization ===" <| guardTest do
    recordSuccess "leanpostgres: FFI initialized OK"

/-- Connects against the live Postgres instance addressed by the standard `PG*` env vars. -/
def testConnectSuccess : TestM Unit :=
  withHeader "=== Testing connect (success) ===" <| guardTest do
    let conn ← «open» ""
    recordSuccess s!"connected: {repr conn}"

/--
Connects to a reachable host on a port nothing listens on, so libpq fails fast with
"connection refused" rather than needing a `connect_timeout` to avoid hanging.
-/
def testConnectFailure : TestM Unit :=
  withHeader "=== Testing connect (failure) ===" <| guardTest do
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
        recordSuccess s!"connect failure correctly surfaced as a Postgres.Error: {pgErr}"

/--
`Conn.isLive` neither reports a healthy connection dead nor damages it. It works by reading
whatever libpq has waiting, so a check that consumed more than it should would leave the
connection broken rather than merely misreported, which only shows up on the next statement.
-/
def testConnIsLiveOnHealthyConnection : TestM Unit :=
  withHeader "=== Testing Conn.isLive on a healthy connection ===" <| guardTest do
    let conn ← «open» ""
    for _ in [:1000] do
      unless ← conn.isLive do
        throw <| IO.userError "healthy connection reported as no longer live"
    let stmt ← prepare conn "SELECT 'still here'"
    unless ← stmt.step do
      throw <| IO.userError "expected a row from a connection reported as live"
    if (← stmt.columnText 0) != "still here" then
      throw <| IO.userError "connection returned a damaged result after repeated liveness checks"
    recordSuccess "healthy connection stayed live and undamaged across 1000 liveness checks"

/--
`Conn.isLive` reports a connection whose backend has been terminated. The termination is done from
a second connection and confirmed server-side, but the close still has to travel back to this
client, so this retries within a bound rather than checking once; a single check immediately after
the backend goes finds nothing has arrived yet.
-/
def testConnIsLiveDetectsTerminatedBackend : TestM Unit :=
  withHeader "=== Testing Conn.isLive detects a terminated backend ===" <| guardTest do
    let victim ← «open» ""
    let observer ← «open» ""
    let pid ← backendPid victim

    unless ← victim.isLive do
      throw <| IO.userError "connection reported dead before its backend was terminated"

    terminateBackend observer pid

    unless ← waitUntilNotLive victim 200 do
      throw <| IO.userError "a terminated backend was never reported by Conn.isLive"
    recordSuccess "terminated backend reported as no longer live"

/--
A single `Conn.isLive` call notices a connection the server has closed. This is what a caller
borrowing from a pool gets: one check, not a retried one. libpq reports success for the read that
takes in the server's parting message, and reports the close only on the read after it, so an
implementation that looks just once concludes every connection is healthy. Retrying conceals that,
which is why this deliberately does not retry.

The connection under test is closed first and a second one closed after it over the same path.
Waiting for the later one to be noticed, which may retry freely, establishes that delivery has
already happened for the earlier one before its single check is made.
-/
def testConnIsLiveNoticesCloseInOneCall : TestM Unit :=
  withHeader "=== Testing Conn.isLive notices a closed connection in one call ===" <| guardTest do
    let subject ← «open» ""
    let canary ← «open» ""
    let observer ← «open» ""
    let subjectPid ← backendPid subject
    let canaryPid ← backendPid canary

    terminateBackend observer subjectPid
    terminateBackend observer canaryPid

    unless ← waitUntilNotLive canary 200 do
      throw <| IO.userError "the canary connection was never reported as closed"

    if ← subject.isLive then
      throw <| IO.userError
        "a single check reported a closed connection as live, so it looked only once"
    recordSuccess "a single liveness check noticed a closed connection"
