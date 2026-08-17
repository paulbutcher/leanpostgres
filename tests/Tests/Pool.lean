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
