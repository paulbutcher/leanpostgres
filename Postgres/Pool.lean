/-
Copyright (c) 2026 Paul Butcher. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module
public import Postgres.LowLevel
public import Std.Sync.Channel
public import Std.Async

set_option doc.verso true
set_option linter.missingDocs true

namespace Postgres

public section

/--
A fixed-size pool of connections, to share safely across concurrent tasks.

libpq connections aren't safe for concurrent use from multiple threads or fibers; two callers
issuing commands on the same {name}`Conn` at once can leave it permanently wedged, since libpq has
no timeout of its own for a socket that's simply waiting on bytes nobody will ever send. A pool
hands out one connection per caller at a time via {lit}`Pool.withConn`/{lit}`Pool.withConnAsync`,
which are the only ways to obtain one; there's no separate acquire/release pair to misuse and leak
a connection that never gets returned.

Built on {name (full := Std.CloseableChannel)}`Std.CloseableChannel`, a bounded,
multi-producer/multi-consumer FIFO channel: {lit}`Pool.create` pre-fills a channel of capacity
{lit}`size` with {lit}`size` open connections; borrowing receives one (waiting if none are free)
and returning sends it back. The pool never closes this channel, so a closed-channel result from it
is an internal invariant violation, not a real error case a caller needs to handle.
-/
structure Pool where
  /-- The connection string every pooled connection was opened with. -/
  conninfo : String
  /-- The number of connections held by the pool. -/
  size : Nat
  private channel : Std.CloseableChannel Conn

namespace Pool

/--
Opens {name}`size` connections against {name}`conninfo`, eagerly and upfront, and returns a pool
that hands them out one at a time via {lit}`Pool.withConn`/{lit}`Pool.withConnAsync`.

{name}`size` must be greater than zero. If any of the {lit}`size` opens fails, the whole call
fails rather than silently returning an undersized pool; any connections already opened before the
failure are simply dropped, finalized the same way any other {name}`Conn` going out of scope is.
-/
def create (conninfo : String) (size : Nat) : IO Pool := do
  if size = 0 then
    throw <| IO.userError "pool size must be greater than 0"
  let channel ← Std.CloseableChannel.new (capacity := some size)
  for _ in [:size] do
    let conn ← «open» conninfo
    channel.sync.send conn
  return { conninfo, size, channel }

private def acquire (pool : Pool) : IO Conn := do
  match ← pool.channel.sync.recv with
  | some conn => return conn
  | none => throw <| IO.userError "Postgres.Pool: connection channel closed unexpectedly"

private def release (pool : Pool) (conn : Conn) : IO Unit :=
  pool.channel.sync.send conn

/--
Runs {name}`action` against a connection borrowed from {name}`pool`, from synchronous
{name (full := IO)}`IO` code.

Acquires a connection, waiting (blocking the calling thread) if none are currently free, runs
{name}`action`, and returns the connection to the pool once {name}`action` completes, whether it
succeeds or throws, then returns or rethrows {name}`action`'s outcome. Modeled on
{name}`Postgres.transaction`'s bracket shape, and for the same reason: it's the only way to use a
pooled connection, so a checked-out connection can never accidentally be left unreturned.

Callers running inside {name (full := Std.Async.Async)}`Std.Async.Async` (e.g. a fiber-multiplexed
HTTP handler) should use {lit}`Pool.withConnAsync` instead. Blocking the underlying OS thread here
while waiting for a free connection would stall every other fiber sharing that thread, a milder
version of the exact hazard this pool exists to prevent.
-/
def withConn (pool : Pool) (action : Conn → IO α) : IO α := do
  let conn ← pool.acquire
  try
    action conn
  finally
    pool.release conn

private def acquireAsync (pool : Pool) : Std.Async.Async Conn := do
  match ← Std.Async.Async.ofIOTask pool.channel.recv with
  | some conn => return conn
  | none => throw <| IO.userError "Postgres.Pool: connection channel closed unexpectedly"

private def releaseAsync (pool : Pool) (conn : Conn) : Std.Async.Async Unit := do
  let task ← pool.channel.send conn
  Std.Async.Async.ofAsyncTask (task.map (Except.mapError (IO.userError ∘ toString)))

/--
Runs {name}`action` against a connection borrowed from {name}`pool`, from
{name (full := Std.Async.Async)}`Std.Async.Async` code.

Same bracket shape and guarantee as {name}`Pool.withConn`, but waits for a free connection
cooperatively rather than by blocking the calling OS thread, so it's safe to call from a
fiber-multiplexed request handler (e.g. one served by Std's HTTP server) without stalling unrelated
concurrent requests sharing that thread while the pool is empty. {name}`action` itself still runs
as ordinary blocking {name (full := IO)}`IO`, since the underlying libpq calls it makes are
synchronous no matter which monad calls them.
-/
def withConnAsync (pool : Pool) (action : Conn → IO α) : Std.Async.Async α := do
  let conn ← pool.acquireAsync
  try
    action conn
  finally
    pool.releaseAsync conn

end Pool

end
end Postgres
