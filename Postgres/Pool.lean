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
One unit of a pool's capacity: either a connection ready to be handed out, or the right to open
one.

Capacity is what the pool holds a fixed number of, not connections. The two came to the same thing
while every connection was opened upfront and none was ever replaced, and they stop being the same
thing as soon as either changes.
-/
private inductive Slot where
  /-- A connection available to be borrowed. -/
  | filled (conn : Conn)
  /-- Capacity with no connection behind it yet. -/
  | vacant

/--
A fixed-capacity pool of connections, to share safely across concurrent tasks.

libpq connections aren't safe for concurrent use from multiple threads or fibers; two callers
issuing commands on the same {name}`Conn` at once can leave it permanently wedged, since libpq has
no timeout of its own for a socket that's simply waiting on bytes nobody will ever send. A pool
hands out one connection per caller at a time via {lit}`Pool.withConn`/{lit}`Pool.withConnAsync`,
which are the only ways to obtain one; there's no separate acquire/release pair to misuse and leak
a connection that never gets returned.

Built on {name (full := Std.CloseableChannel)}`Std.CloseableChannel`, a bounded,
multi-producer/multi-consumer FIFO channel holding {lit}`size` units of capacity. Borrowing
receives one (waiting if none are free) and opens a connection if that unit doesn't already carry
one; returning sends it back. Connections are therefore established by the borrows that need them
rather than all at once, so creating a pool costs one connection rather than {lit}`size`. Capacity
is recycled in the order it's returned, though, so a pool that serves at least {lit}`size` borrows
ends up holding {lit}`size` connections whether or not it was ever busy enough to need them all at
once.

Every path out of a borrow puts exactly one unit back, including the path where opening a
connection failed, so {lit}`size` is invariant: a pool whose database has been unreachable for any
length of time still admits exactly {lit}`size` concurrent callers once it returns. The pool never
closes the channel, so a closed-channel result from it is an internal invariant violation, not a
real error case a caller needs to handle.
-/
structure Pool where
  /-- The connection string every pooled connection is opened with. -/
  conninfo : String
  /-- The number of connections the pool will hold at once. -/
  size : Nat
  private channel : Std.CloseableChannel Slot

/-- Options for {lit}`Pool.create`. -/
structure PoolOptions where
  /--
  Whether {lit}`Pool.create` must establish a connection before returning.

  Leaving this set makes a connection string that cannot work fail at startup, where it is cheap to
  notice and usually worth failing a deployment over. Clearing it lets a pool be created while the
  database is merely unreachable, so a process starting during a failover comes up and recovers
  instead of failing permanently; the first error then reaches the first borrow rather than the
  caller of {lit}`Pool.create`, which is a worse place to learn about a typo and a better one to
  learn about an outage.
  -/
  requireConnection : Bool := true
deriving Repr, BEq, Hashable, Inhabited

namespace Pool

/--
Creates a pool of capacity {name}`size` against {name}`conninfo`, handing connections out one at a
time via {lit}`Pool.withConn`/{lit}`Pool.withConnAsync`.

{name}`size` must be greater than zero. At most one connection is opened here, to establish that
{name}`conninfo` works at all; the rest are opened by the borrows that need them. See
{name}`PoolOptions.requireConnection` for what happens when that one connection can't be opened.
-/
def create (conninfo : String) (size : Nat) (opts : PoolOptions := {}) : IO Pool := do
  if size = 0 then
    throw <| IO.userError "pool size must be greater than 0"
  let channel ← Std.CloseableChannel.new (capacity := some size)
  let first ← if opts.requireConnection then Slot.filled <$> «open» conninfo else pure .vacant
  channel.sync.send first
  for _ in [1:size] do
    channel.sync.send .vacant
  return { conninfo, size, channel }

/--
Opens a connection for a unit of capacity the caller already holds.

A failed open returns that unit to the pool vacant rather than dropping it. Dropping it would
shrink the pool by one per failure, so a database unreachable for long enough would leave a pool
that is permanently empty and every later borrow waiting on a channel nothing will ever be sent
to, long after the database itself came back.
-/
private def fill (pool : Pool) : IO Conn := do
  try
    «open» pool.conninfo
  catch e =>
    -- Cannot block: the caller holds a unit out, so the channel is below its capacity.
    pool.channel.sync.send .vacant
    throw e

private def acquire (pool : Pool) : IO Conn := do
  match ← pool.channel.sync.recv with
  | none => throw <| IO.userError "Postgres.Pool: connection channel closed unexpectedly"
  | some (.filled conn) => return conn
  | some .vacant => pool.fill

private def release (pool : Pool) (conn : Conn) : IO Unit :=
  pool.channel.sync.send (.filled conn)

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
  | none => throw <| IO.userError "Postgres.Pool: connection channel closed unexpectedly"
  | some (.filled conn) => return conn
  | some .vacant => pool.fill

private def releaseAsync (pool : Pool) (conn : Conn) : Std.Async.Async Unit := do
  let task ← pool.channel.send (.filled conn)
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
