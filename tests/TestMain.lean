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

def main : IO Unit := do
  checkFFIInitialized
  checkConnectSuccess
  checkConnectFailure
  IO.println "all checks passed"
