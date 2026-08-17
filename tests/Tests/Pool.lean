/-
Copyright (c) 2026 Paul Butcher. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Postgres
import PostgresTest.Framework
import Tests.Helpers
import Plausible

open Postgres
open Postgres.Test
open Plausible

/--
Runs `borrowers` concurrent `withConn` calls against `pool` (real concurrency, via `IO.asTask`),
each holding its connection just long enough (`IO.sleep`) to make overlap likely, and tracks the
live checked-out count in a shared `IO.Ref`. Returns the highest checked-out count observed and
whether every borrower completed without throwing.
-/
def runPoolBorrowers (pool : Pool) (borrowers : Nat) : IO (Nat × Bool) := do
  let checkedOut ← IO.mkRef 0
  let maxObserved ← IO.mkRef 0
  let tasks ← (List.range borrowers).toArray.mapM fun _ =>
    IO.asTask <| pool.withConn fun _conn => do
      let current ← checkedOut.modifyGet fun c => (c + 1, c + 1)
      maxObserved.modify (max · current)
      IO.sleep 20
      checkedOut.modify (· - 1)
  let mut allOk := true
  for task in tasks do
    match ← IO.wait task with
    | .ok _ => pure ()
    | .error _ => allOk := false
  return (← maxObserved.get, allOk)

/--
Spawns `size + 2` concurrent `withConn` borrowers against a pool of `size` connections. The pool
must never let the checked-out count exceed `size`, and every borrower (including the extras that
had to wait for a connection to free up) must complete rather than deadlock.
-/
def testPoolConcurrencyBound : TestM Unit :=
  withHeader "=== Testing Pool.withConn never exceeds its size concurrently ===" <| guardTest do
    let size := 3
    let borrowers := size + 2
    let pool ← Pool.create "" size
    let (maxObserved, allOk) ← runPoolBorrowers pool borrowers
    if !allOk then
      throw <| IO.userError "expected every borrower to complete, but at least one threw"
    if maxObserved > size then
      throw <| IO.userError
        s!"expected at most {size} concurrently checked-out connections, observed {maxObserved}"
    recordSuccess
      s!"pool concurrency bound held (size={size}, borrowers={borrowers}, maxObserved={maxObserved})"

/--
A `withConn` call whose action throws must still return its connection to the pool; proven by
following it immediately with another `withConn` against a size-1 pool, which can only complete
promptly if the first borrow's connection was actually returned rather than lost.
-/
def testPoolReleaseOnThrow : TestM Unit :=
  withHeader "=== Testing Pool.withConn returns a connection even when action throws ===" <| guardTest do
    let pool ← Pool.create "" 1
    let failingAction : Conn → IO Unit := fun _ => throw <| IO.userError "boom"
    let threw ← try
        pool.withConn failingAction
        pure false
      catch _ => pure true
    if !threw then throw <| IO.userError "expected withConn to rethrow the action's exception"
    pool.withConn (fun _ => pure () : Conn → IO Unit)
    recordSuccess "connection correctly returned to the pool after a throwing action"

/--
Bounded generator for `(size, extraBorrowers)` pairs: pool sizes stay small (1..4) and the number
of extra borrowers beyond `size` stays small too (0..4), since each generated pair drives a real
pool of live connections rather than a pure computation.
-/
def poolShapeGen : Gen (Nat × Nat) := do
  let ⟨size, _⟩ ← Gen.choose Nat 1 4 (by omega)
  let ⟨extra, _⟩ ← Gen.choose Nat 0 4 (by omega)
  return (size, extra)

/--
Property counterpart to `testPoolConcurrencyBound`: "for any pool size and any number of
concurrent borrowers, the checked-out count never exceeds the pool size" is a real invariant, not
a restated literal, and Plausible's generators are a natural fit for sampling the `(size,
borrowers)` space. `Testable.checkIO` can't drive this directly, though: generating a sample runs
in the effect-free `Gen` monad, while exercising the invariant needs real `IO` against a live
pool. This instead drives `Gen` sampling by hand, running one live check per sample.
-/
def testPoolSizeInvariantProperty : TestM Unit :=
  withHeader "=== Testing Pool.withConn size invariant (property-based) ===" <| guardTest do
    for _ in [:15] do
      let (size, extra) ← poolShapeGen.run 0
      let borrowers := size + extra
      let pool ← Pool.create "" size
      let (maxObserved, allOk) ← runPoolBorrowers pool borrowers
      if !allOk then
        throw <| IO.userError
          s!"expected every borrower to complete (size={size}, borrowers={borrowers}), but at least one threw"
      if maxObserved > size then
        throw <| IO.userError
          s!"pool size invariant violated: size={size}, borrowers={borrowers}, maxObserved={maxObserved}"
    recordSuccess "pool size invariant held across 15 generated (size, borrower-count) pairs"

/--
A pool created without establishing a connection is fully usable: the connections it hands out are
opened by the borrows that need them, and it still bounds concurrency to its capacity.
-/
def testPoolCreatedWithoutConnecting : TestM Unit :=
  withHeader "=== Testing a pool created without connecting ===" <| guardTest do
    let size := 3
    let pool ← Pool.create "" size { requireConnection := false }
    let (maxObserved, allOk) ← runPoolBorrowers pool (size + 2)
    if !allOk then
      throw <| IO.userError "expected every borrower against a lazily filled pool to complete"
    if maxObserved > size then
      throw <| IO.userError
        s!"expected at most {size} concurrently checked-out connections, observed {maxObserved}"
    recordSuccess s!"pool created without connecting served {size + 2} borrowers"

/--
A pool that has been busy and then quietens settles back onto one connection, rather than
continuing to rotate through everything it opened at its peak. Returning the least recently used
connection instead would keep every connection in service indefinitely, so a burst of traffic would
permanently commit the pool, and the server, to its high-water mark.

Backend process ids identify the connections. The concurrent phase exists to put more than one
connection in the pool, without which the check that follows would hold whatever the order.
-/
def testPoolSettlesOntoOneConnection : TestM Unit :=
  withHeader "=== Testing a pool settles back onto one connection ===" <| guardTest do
    let size := 4
    let pool ← Pool.create "" size

    let tasks ← (List.range size).toArray.mapM fun _ =>
      IO.asTask <| pool.withConn fun conn => do
        let pid ← backendPid conn
        IO.sleep 20
        pure pid
    let mut opened : Array String := #[]
    for task in tasks do
      match ← IO.wait task with
      | .ok pid => opened := opened.push pid
      | .error e => throw e
    let some firstOpened := opened[0]?
      | throw <| IO.userError "expected the concurrent phase to borrow at least once"
    unless opened.any (· != firstOpened) do
      throw <| IO.userError
        "concurrent borrows all landed on one connection, so the pool never held more than one and the check below would prove nothing"

    let borrows := size * 2
    let mut pids : Array String := #[]
    for _ in [:borrows] do
      pids := pids.push (← pool.withConn backendPid)
    let some first := pids[0]?
      | throw <| IO.userError "expected at least one borrow"
    unless pids.all (· == first) do
      throw <| IO.userError
        s!"expected {borrows} non-overlapping borrows to settle on one connection, saw backends {pids}"
    recordSuccess s!"a pool of {size} settled back onto a single connection after being busy"

/--
Creating a pool requires a connection unless the caller says otherwise. A connection string that
can never work fails at startup by default, where a deployment can notice; an application that has
to survive starting while its database is unreachable opts out, and meets the failure at its first
borrow instead.
-/
def testPoolCreateConnectionRequirement : TestM Unit :=
  withHeader "=== Testing Pool.create's connection requirement ===" <| guardTest do
    let threw ← try
        let _ ← Pool.create unreachableConninfo 3
        pure false
      catch _ => pure true
    unless threw do
      throw <| IO.userError
        "expected Pool.create to fail by default when the database is unreachable"

    let pool ← Pool.create unreachableConninfo 3 { requireConnection := false }
    let caught ← try
        pool.withConn (fun _ => pure () : Conn → IO Unit)
        pure (none : Option IO.Error)
      catch e => pure (some e)
    if caught.isNone then
      throw <| IO.userError "expected borrowing against an unreachable database to fail"
    recordSuccess "Pool.create requires a connection by default, and defers the failure when told not to"

/--
A connection closed server-side while it sat in the pool is replaced, so the next borrow gets one
that works. This is the whole point of the exercise: an application idle across a database restart
must not serve the next request an error.

A canary connection is closed after the pool's and waited for, which establishes that the close of
the pool's connection has arrived before the borrow that has to notice it. Nothing here waits on a
timeout; the wait is for a few milliseconds of network delivery.
-/
def testPoolReplacesClosedConnection : TestM Unit :=
  withHeader "=== Testing a pool replaces a connection closed while idle ===" <| guardTest do
    let pool ← Pool.create "" 1
    let observer ← «open» ""
    let pooledPid ← pool.withConn backendPid

    let canary ← «open» ""
    let canaryPid ← backendPid canary
    terminateBackend observer pooledPid
    terminateBackend observer canaryPid
    unless ← waitUntilNotLive canary 200 do
      throw <| IO.userError "the canary connection was never reported as closed"

    let replacedPid ← pool.withConn backendPid
    if replacedPid == pooledPid then
      throw <| IO.userError "expected the closed connection to have been replaced"
    recordSuccess s!"pool replaced a connection closed while idle (backend {pooledPid} to {replacedPid})"

/--
A statement the server rejects is not a reason to replace a connection. The error must reach the
caller with its SQLSTATE, and the connection it happened on must still be the one the pool holds:
treating every failure as a connection failure would silently discard a working connection on every
constraint violation, and would be invisible except as unexplained reconnections.
-/
def testPoolKeepsConnectionAfterServerError : TestM Unit :=
  withHeader "=== Testing a pool keeps its connection after a server-side error ===" <| guardTest do
    let pool ← Pool.create "" 1
    let before ← pool.withConn backendPid

    let caught ← try
        pool.withConn (fun conn => do (← prepare conn "SELECT 1 / 0").exec)
        pure (none : Option IO.Error)
      catch e => pure (some e)
    let some err := caught
      | throw <| IO.userError "expected a rejected statement to reach the caller"
    let some parsed := Error.ofIOError? err
      | throw <| IO.userError s!"rejected statement reached the caller with no SQLSTATE: {err}"
    if parsed.sqlstate != "22012" then
      throw <| IO.userError s!"expected SQLSTATE 22012, got '{parsed.sqlstate}'"

    let after ← pool.withConn backendPid
    if after != before then
      throw <| IO.userError
        s!"a statement the server rejected caused the connection to be replaced ({before} to {after})"
    recordSuccess s!"server-side error surfaced as {parsed.sqlstate} and the connection was kept"

/--
An exception raised by the caller's own code, having nothing to do with the connection, is not a
reason to replace it either.
-/
def testPoolKeepsConnectionAfterCallerError : TestM Unit :=
  withHeader "=== Testing a pool keeps its connection after a caller's own error ===" <| guardTest do
    let pool ← Pool.create "" 1
    let before ← pool.withConn backendPid
    try
      pool.withConn (fun _ => throw (IO.userError "boom") : Conn → IO Unit)
    catch _ => pure ()
    let after ← pool.withConn backendPid
    if after != before then
      throw <| IO.userError
        s!"a caller's own exception caused the connection to be replaced ({before} to {after})"
    recordSuccess "connection kept after an exception unrelated to it"

/--
A borrow from a pool in steady use sends nothing to the server. Checking a borrowed connection is
supposed to be free, and an implementation that checked by sending something would meet every other
requirement while adding a round trip to every request of an application whose database is next
door.

What the server last saw on the backend answers this without sending anything to find out, so the
second borrow does no work of its own: anything the server has seen since must have come from the
pool. The observation happens inside that borrow, since a pool with no live reference left is
finalized, taking its connections with it.
-/
def testPoolHotBorrowSendsNothing : TestM Unit :=
  withHeader "=== Testing a borrow from a pool in steady use sends nothing ===" <| guardTest do
    let observer ← «open» ""
    let pool ← Pool.create "" 1 { validateAfterIdle := none }
    let pid ← pool.withConn backendPid
    pool.withConn fun _ => do
      let seen ← lastQuery observer pid
      unless seen == "SELECT pg_backend_pid()" do
        throw <| IO.userError
          s!"expected the server to have seen nothing since the previous borrow, it last saw '{seen}'"
    recordSuccess "borrow from a pool in steady use sent nothing to the server"

/--
A connection idle beyond the configured threshold is checked by sending something, which is the
only way to tell a working connection from one whose flow has been dropped silently.

The threshold is zero here so that the check always applies; the point being tested is that the
threshold is consulted at all, not how long it is.
-/
def testPoolChecksConnectionPastIdleThreshold : TestM Unit :=
  withHeader "=== Testing a pool checks a connection past its idle threshold ===" <| guardTest do
    let observer ← «open» ""
    let pool ← Pool.create "" 1 { validateAfterIdle := some (Std.Time.Duration.ofSeconds 0) }
    let pid ← pool.withConn backendPid
    let seen ← pool.withConn fun _ => lastQuery observer pid
    if seen == "SELECT pg_backend_pid()" then
      throw <| IO.userError
        "expected the pool to have sent something on a connection past its idle threshold"
    recordSuccess s!"pool checked a connection past its idle threshold (server last saw '{seen}')"

/--
A pool's counters separate a database that is quietly working from one that is flapping. Both serve
every request, so without something to read there is nothing to tell them apart by.

A healthy borrow must move nothing, or the counters would rise steadily whatever the database was
doing and say as little as no counters at all.
-/
def testPoolStatisticsCountReplacement : TestM Unit :=
  withHeader "=== Testing pool statistics count a replacement ===" <| guardTest do
    let pool ← Pool.create "" 1
    let observer ← «open» ""
    let pooledPid ← pool.withConn backendPid

    let quiet ← pool.statistics
    let _ ← pool.withConn backendPid
    unless (← pool.statistics) == quiet do
      throw <| IO.userError "a borrow that replaced nothing still moved the counters"

    let canary ← «open» ""
    let canaryPid ← backendPid canary
    terminateBackend observer pooledPid
    terminateBackend observer canaryPid
    unless ← waitUntilNotLive canary 200 do
      throw <| IO.userError "the canary connection was never reported as closed"

    let _ ← pool.withConn backendPid
    let after ← pool.statistics
    if after.discarded != quiet.discarded + 1 then
      throw <| IO.userError
        s!"expected one discarded connection, went from {quiet.discarded} to {after.discarded}"
    if after.opened != quiet.opened + 1 then
      throw <| IO.userError
        s!"expected one connection opened to replace it, went from {quiet.opened} to {after.opened}"
    if after.openFailures != quiet.openFailures then
      throw <| IO.userError "a replacement that succeeded was counted as a failure"
    recordSuccess "pool statistics counted a successful replacement and ignored a healthy borrow"

/--
A replacement that could not be completed is counted separately from one that could, which is the
distinction that matters: a pool discarding connections and reopening them is coping, and a pool
discarding them and failing to reopen is not.
-/
def testPoolStatisticsCountOpenFailure : TestM Unit :=
  withHeader "=== Testing pool statistics count a failed open ===" <| guardTest do
    let pool ← Pool.create unreachableConninfo 1 { requireConnection := false }
    let before ← pool.statistics
    try
      pool.withConn (fun _ => pure () : Conn → IO Unit)
    catch _ => pure ()
    let after ← pool.statistics
    if after.openFailures != before.openFailures + 1 then
      throw <| IO.userError
        s!"expected one failed open, went from {before.openFailures} to {after.openFailures}"
    if after.opened != before.opened then
      throw <| IO.userError "a failed open was counted as a connection opened"
    recordSuccess "pool statistics counted a failed open without counting it as a success"

/--
Capacity survives failed opens. A pool that gives up a unit of capacity whenever an open fails
empties during an outage and then blocks every later borrow indefinitely, a failure that outlives
the outage that caused it and looks nothing like it.

Every borrow here fails, many times over the pool's capacity; the pool must still admit `size`
concurrent callers afterwards.

When this test does fail, the run hangs after reporting it, because borrowers left waiting on a
pool that has lost capacity keep the process from exiting. That is unavoidable: establishing that
borrows no longer block requires borrows that would block if they did. The recorded failure is
printed before the hang.
-/
def testPoolCapacitySurvivesFailedOpens : TestM Unit :=
  withHeader "=== Testing pool capacity survives failed opens ===" <| guardTest do
    let size := 3
    let attempts := size * 3
    let pool ← Pool.create unreachableConninfo size { requireConnection := false }

    -- Every phase waits with a bound rather than blocking. A pool that loses capacity leaves
    -- borrowers waiting on a channel nothing will be sent to, so blocking on them would hang the
    -- suite instead of failing it.
    let failing ← (List.range attempts).toArray.mapM fun _ =>
      IO.asTask <| try
          pool.withConn (fun _ => pure () : Conn → IO Unit)
          pure false
        catch _ => pure true
    unless ← waitForTasks failing 500 do
      throw <| IO.userError
        s!"only some of {attempts} borrows completed, so the pool is losing capacity per failed open"
    for task in failing do
      match task.get with
      | .ok true => pure ()
      | .ok false => throw <| IO.userError "expected every borrow against an unreachable database to fail"
      | .error e => throw e

    let after ← (List.range size).toArray.mapM fun _ =>
      IO.asTask <| try pool.withConn (fun _ => pure () : Conn → IO Unit) catch _ => pure ()
    unless ← waitForTasks after 500 do
      throw <| IO.userError
        s!"pool stopped admitting {size} concurrent callers after {attempts} failed opens"
    recordSuccess s!"capacity of {size} survived {attempts} failed opens"
