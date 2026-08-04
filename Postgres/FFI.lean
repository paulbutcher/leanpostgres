/-
Copyright (c) 2026 Paul Butcher. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module
namespace Postgres.FFI

@[extern "leanpostgres_initialize"]
private opaque initModule : IO Unit
builtin_initialize initModule

opaque T : NonemptyType.{0}

instance : Nonempty T.type := T.property

public section

/-- Opaque wrapper around a `PGconn*`. -/
def Conn : Type := T.type
deriving Nonempty

public instance : Repr Conn where
  reprPrec _ _ := "#<PGconn *>"

/-- Opaque wrapper around a `PGresult*`. -/
def Result : Type := T.type
deriving Nonempty

public instance : Repr Result where
  reprPrec _ _ := "#<PGresult *>"

@[extern "leanpostgres_open"]
opaque «open» : String → IO Conn

/--
Executes `sql` with the given parameters via `PQexecParams` (text format throughout; every
element of `params` is either `none` for SQL `NULL` or `some` already-encoded text). Returns the
buffered result set, or throws a `Postgres.Error`-shaped error if `PQresultStatus` isn't
`PGRES_TUPLES_OK`/`PGRES_COMMAND_OK`.
-/
@[extern "leanpostgres_exec_params"]
private opaque execParams : @&Conn → String → Array (Option String) → IO Result

/-- The number of rows in a buffered result set. -/
@[extern "leanpostgres_ntuples"]
private opaque ntuples : @&Result → Int32

/-- The number of columns in a buffered result set. -/
@[extern "leanpostgres_nfields"]
private opaque nfields : @&Result → Int32

/-- The text value of `(row, column)` in a buffered result set. Meaningless if the cell is NULL. -/
@[extern "leanpostgres_getvalue"]
private opaque getvalue : @&Result → Int32 → Int32 → IO String

/-- Whether `(row, column)` is SQL `NULL` in a buffered result set. -/
@[extern "leanpostgres_getisnull"]
private opaque getisnull : @&Result → Int32 → Int32 → Bool

/-- The name of a result column (0-indexed), i.e. its output name/alias. -/
@[extern "leanpostgres_fname"]
private opaque fname : @&Result → Int32 → IO String

/-- The command tag of the executed statement (e.g. `"SELECT"`, `"INSERT 0 3"`). -/
@[extern "leanpostgres_cmd_status"]
private opaque cmdStatus : @&Result → IO String

/-- The number of rows affected, as decimal text; empty if the command doesn't produce one. -/
@[extern "leanpostgres_cmd_tuples"]
private opaque cmdTuples : @&Result → IO String

/-- The OID of the table a result column directly references, or `0` if it's a computed expression. -/
@[extern "leanpostgres_ftable"]
private opaque ftable : @&Result → Int32 → UInt32

/-- The attribute number within the table `ftable` identifies, or `0` if none. -/
@[extern "leanpostgres_ftablecol"]
private opaque ftablecol : @&Result → Int32 → Int32

/-- The connection's current database name. -/
@[extern "leanpostgres_db"]
private opaque db : @&Conn → IO String

-- --- Non-blocking query execution, driven by `Postgres.Async`'s poller loop. ---

/--
Switches `conn` into (`enable = true`) or out of (`enable = false`) libpq's non-blocking mode.
Required before `sendQueryParams`/`flush`/`consumeInput`/`getResult` will avoid blocking; sync
`step`/`exec` (via `execParams`) is documented by libpq as unreliable on a connection left in
non-blocking mode, so callers must restore blocking mode once done.
-/
@[extern "leanpostgres_set_nonblocking"]
private opaque setNonblocking : @&Conn → Bool → IO Unit

/-- The connection's underlying socket file descriptor, to wait on via `poll`. -/
@[extern "leanpostgres_socket"]
private opaque socket : @&Conn → Int32

/--
Queues `sql`/`params` for asynchronous execution via `PQsendQueryParams`; returns once the command
has been queued, not once it's complete. `conn` must already be in non-blocking mode (`setNonblocking
conn true`). Same parameter encoding as `execParams`.
-/
@[extern "leanpostgres_send_query_params"]
private opaque sendQueryParams : @&Conn → String → Array (Option String) → IO Unit

/--
Attempts to send any data still buffered from `sendQueryParams`. Returns `true` if more remains to
be flushed once the socket is next writable, `false` once fully flushed.
-/
@[extern "leanpostgres_flush"]
private opaque flush : @&Conn → IO Bool

/--
Reads whatever is currently available from the socket into libpq's internal buffers; doesn't block.
Call once the socket is reported readable, then re-check `isBusy`.
-/
@[extern "leanpostgres_consume_input"]
private opaque consumeInput : @&Conn → IO Unit

/--
Whether fetching the result via `getResult` would currently block. Just inspects already-buffered
state (no socket I/O), so unlike `flush`/`consumeInput` this can't fail.
-/
@[extern "leanpostgres_is_busy"]
private opaque isBusy : @&Conn → Bool

/--
Fetches the result of the command sent via `sendQueryParams`, once `isBusy` reports `false`. Same
error/status-checking behavior as `execParams`.
-/
@[extern "leanpostgres_get_result"]
private opaque getResult : @&Conn → IO Result

-- --- Generic socket-readiness polling, backing `Postgres.Async`'s single poller loop. ---

/--
Blocks until at least one of `fds[i]` becomes ready for reading (`wantWrite[i] = false`) or writing
(`wantWrite[i] = true`), or `wake` is called from another thread. Returns a same-length array of
which entries are ready. `fds` and `wantWrite` must be the same length.
-/
@[extern "leanpostgres_poll"]
private opaque poll : Array Int32 → Array Bool → IO (Array Bool)

/-- Wakes a `poll` call blocked in another thread, e.g. after registering a new wait. -/
@[extern "leanpostgres_wake"]
private opaque wake : IO Unit
