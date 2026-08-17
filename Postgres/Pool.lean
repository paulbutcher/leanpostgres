/-
Copyright (c) 2026 Paul Butcher. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module
public import Postgres.LowLevel
public import Std.Sync.Channel
public import Std.Sync.Mutex
public import Std.Async
public import Std.Time

set_option doc.verso true
set_option linter.missingDocs true

namespace Postgres

public section

/--
Running totals of what a pool has had to do to keep working, none of which an application can
otherwise see.

A database that restarts every few seconds and one that never restarts both serve every request
successfully, and are told apart only by how much work that took. The counts only ever rise;
what's informative is how fast.

A replacement that worked shows up as {name}`PoolStats.discarded` and {name}`PoolStats.opened`
rising together. One that failed shows up as {name}`PoolStats.discarded` and
{name}`PoolStats.openFailures` rising together instead, and a borrow that found nothing to discard
and still couldn't open shows up as {name}`PoolStats.openFailures` alone.
-/
structure PoolStats where
  /-- Connections successfully opened, counting the pool's first and every replacement since. -/
  opened : Nat
  /-- Connections found unusable when borrowed, and discarded rather than handed over. -/
  discarded : Nat
  /-- Attempts to open a connection that failed. -/
  openFailures : Nat
deriving Repr, BEq, Inhabited

/-- A connection nobody is using, alongside readings of both clocks from when it was returned. -/
private structure Idle where
  conn : Conn
  mono : Nat
  wall : Int

/-- Both clocks, read together. -/
private def readClocks : IO (Nat × Int) := do
  let mono ← IO.monoNanosNow
  let wall ← Std.Time.Timestamp.now
  return (mono, wall.toNanosecondsSinceUnixEpoch.toInt)

/--
How long a connection has been idle, in nanoseconds, taken as the greater of what the two clocks
report.

Neither clock can be trusted alone. A monotonic clock doesn't advance while the process is
suspended, so a connection idle across a frozen execution environment looks untouched by it, which
is precisely the case worth catching. A wall clock can be stepped backwards by an adjustment. The
two errors aren't equally bad either: over-reporting costs one unnecessary round trip, while
under-reporting hands a caller a connection that doesn't work, so the larger reading is the safe
one.
-/
private def Idle.elapsedNanos (entry : Idle) : IO Nat := do
  let (mono, wall) ← readClocks
  return max (mono - entry.mono) (wall - entry.wall).toNat

/--
A fixed-capacity pool of connections, to share safely across concurrent tasks.

libpq connections aren't safe for concurrent use from multiple threads or fibers; two callers
issuing commands on the same {name}`Conn` at once can leave it permanently wedged, since libpq has
no timeout of its own for a socket that's simply waiting on bytes nobody will ever send. A pool
hands out one connection per caller at a time via {lit}`Pool.withConn`/{lit}`Pool.withConnAsync`,
which are the only ways to obtain one; there's no separate acquire/release pair to misuse and leak
a connection that never gets returned.

Capacity and connections are tracked separately. A
{name (full := Std.CloseableChannel)}`Std.CloseableChannel` holds {lit}`size` permits, one per unit
of capacity, and is what bounds concurrency and what a caller waits on; the connections themselves
sit in a stack of the ones nobody is using. Borrowing takes a permit, waiting if none are free,
then takes the most recently returned connection, opening one only when the stack is empty.
Returning pushes the connection back and releases the permit.

Connections are therefore established by the borrows that need them rather than all at once, so
creating a pool costs one connection rather than {lit}`size`, and a pool serving one caller at a
time only ever opens one however large its capacity. Taking the most recently returned connection
rather than the least is what makes a pool that has been busy settle back onto one afterwards: the
connections a burst forced it to open fall out of use instead of being kept in rotation, leaving
the server free to reclaim them.

Every path out of a borrow releases exactly one permit, including the path where opening a
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
  private permits : Std.CloseableChannel Unit
  private idle : Std.Mutex (List Idle)
  private idleThresholdNanos : Option Nat
  private stats : Std.Mutex PoolStats

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
  /--
  How long a connection may sit idle before the pool checks it with a statement rather than only
  locally, or {lean}`none` never to do so.

  Checking a borrowed connection locally is free and catches one the server closed, but a
  connection whose network path dropped the flow silently looks exactly like a working one until
  something is actually sent. Sending something is the only way to tell, and it costs a round trip,
  so it's worth doing on a connection that has been sitting long enough to have gone stale and not
  on one returned moments ago. A pool in steady use therefore pays nothing for this.

  How long that round trip takes to fail when the connection really is gone isn't bounded by
  anything here: without {lit}`tcp_user_timeout` or the {lit}`keepalives_*` parameters set in
  {name (full := Pool.conninfo)}`conninfo`, the operating system will retransmit for many minutes
  before giving up. Setting them is what makes this check finish in time to be useful, and only the
  application knows what value suits the network between it and its database.
  -/
  validateAfterIdle : Option Std.Time.Duration := some (Std.Time.Duration.ofSeconds 30)
deriving Repr, Inhabited

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
  let initial ← if opts.requireConnection then
      let conn ← «open» conninfo
      let (mono, wall) ← readClocks
      pure [({ conn, mono, wall } : Idle)]
    else pure []
  let permits ← Std.CloseableChannel.new (capacity := some size)
  for _ in [:size] do
    permits.sync.send ()
  let idle ← Std.Mutex.new initial
  let idleThresholdNanos := opts.validateAfterIdle.map (·.toNanoseconds.toInt.toNat)
  let stats ← Std.Mutex.new
    { opened := initial.length, discarded := 0, openFailures := 0 : PoolStats }
  return { conninfo, size, permits, idle, idleThresholdNanos, stats }

/-- A snapshot of what {name}`pool` has had to do to keep working. See {name}`PoolStats`. -/
def statistics (pool : Pool) : IO PoolStats :=
  pool.stats.atomically get

private def record (pool : Pool) (f : PoolStats → PoolStats) : IO Unit :=
  pool.stats.atomically (modify f)

/--
Takes the most recently returned idle connection, or {lean}`none` if there are none.

The lock is held only for the length of a list operation, never across anything that waits, so
taking it here can't stall a fiber that {lit}`Pool.withConnAsync` is multiplexing.
-/
private def takeIdle (pool : Pool) : IO (Option Idle) :=
  pool.idle.atomically do
    match ← get with
    | [] => return none
    | entry :: rest => set rest; return some entry

private def putIdle (pool : Pool) (conn : Conn) : IO Unit := do
  let (mono, wall) ← readClocks
  pool.idle.atomically (modify ({ conn, mono, wall } :: ·))

/--
Opens a connection for a permit the caller already holds.

A failed open releases that permit rather than swallowing it. Swallowing it would shrink the pool
by one per failure, so a database unreachable for long enough would leave a pool that is
permanently empty and every later borrow waiting on a channel nothing will ever be sent to, long
after the database itself came back.
-/
private def fill (pool : Pool) : IO Conn := do
  try
    let conn ← «open» pool.conninfo
    pool.record (fun s => { s with opened := s.opened + 1 })
    return conn
  catch e =>
    -- Cannot block: the caller holds a permit, so the channel is below its capacity.
    pool.permits.sync.send ()
    pool.record (fun s => { s with openFailures := s.openFailures + 1 })
    throw e

/-- Throws away a connection that can't be used and opens its replacement. -/
private def replace (pool : Pool) : IO Conn := do
  pool.record (fun s => { s with discarded := s.discarded + 1 })
  pool.fill

/--
Whether an idle connection has been sitting long enough to be worth sending something to, and has
then failed to answer.

A connection returned recently is taken on trust, which is what keeps a pool in steady use from
paying a round trip per borrow. Anything the server rejects counts as a failure to answer, which
also disposes of a connection left in an aborted transaction: it is live, it is reachable, and
every statement on it would fail until someone rolled it back.
-/
private def staleAfterIdle (pool : Pool) (entry : Idle) : IO Bool := do
  let some threshold := pool.idleThresholdNanos | return false
  if (← entry.elapsedNanos) < threshold then return false
  try
    (← prepare entry.conn "SELECT 1").exec
    return false
  catch _ => return true

/--
Turns a permit the caller already holds into a connection that was usable at the moment it was
handed over.

An idle connection the server has closed is dropped and replaced here, which is the same operation
as opening one for a permit that had none: a connection that fails its check is simply not a
connection the pool has. Replacing happens before the caller sees anything, so nothing the caller
runs is ever repeated, and no handle the caller derives can outlive the connection it came from.
-/
private def connectionFor (pool : Pool) : IO Conn := do
  match ← pool.takeIdle with
  | none => pool.fill
  | some entry =>
    if ← entry.conn.isLive then
      if ← pool.staleAfterIdle entry then pool.replace else return entry.conn
    else
      pool.replace

private def acquire (pool : Pool) : IO Conn := do
  match ← pool.permits.sync.recv with
  | none => throw <| IO.userError "Postgres.Pool: connection channel closed unexpectedly"
  | some () => pool.connectionFor

private def release (pool : Pool) (conn : Conn) : IO Unit := do
  -- The connection goes back before the permit, so a caller taking the permit finds it there
  -- rather than opening a second connection while this one sits idle.
  pool.putIdle conn
  pool.permits.sync.send ()

/--
Runs {name}`action` against a connection borrowed from {name}`pool`, from synchronous
{name (full := IO)}`IO` code.

Acquires a connection, waiting (blocking the calling thread) if none are currently free, runs
{name}`action`, and returns the connection to the pool once {name}`action` completes, whether it
succeeds or throws, then returns or rethrows {name}`action`'s outcome. Modeled on
{name}`Postgres.transaction`'s bracket shape, and for the same reason: it's the only way to use a
pooled connection, so a checked-out connection can never accidentally be left unreturned.

A connection the server closed while it sat idle in the pool is replaced before {name}`action` ever
sees it, so an application that stopped issuing statements for a while, or whose database restarted
underneath it, doesn't have to detect that or retry for itself. Replacement happens only at this
point, never during {name}`action`: nothing {name}`action` has already run is repeated, so a
statement that may have taken effect can't be applied twice, and a {name}`Postgres.Stmt` built from
the connection can't be left pointing at one that has been swapped out. An error the server itself
returned, such as a constraint violation, isn't a reason to replace anything and reaches the caller
untouched.

A connection dropped silently by the network, with no close delivered, can't be told apart from a
working one without sending something; see {name}`Postgres.Conn.isLive`. Bounding how long that
takes to discover is what libpq's {lit}`tcp_user_timeout` and {lit}`keepalives_*` connection
parameters are for, and they have to be set in {name (full := Pool.conninfo)}`conninfo` by the
application.

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
  match ← Std.Async.Async.ofIOTask pool.permits.recv with
  | none => throw <| IO.userError "Postgres.Pool: connection channel closed unexpectedly"
  | some () => pool.connectionFor

private def releaseAsync (pool : Pool) (conn : Conn) : Std.Async.Async Unit := do
  pool.putIdle conn
  let task ← pool.permits.send ()
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
