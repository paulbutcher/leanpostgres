/-
Copyright (c) 2026 Paul Butcher. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Postgres
import PostgresTest.Framework

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
