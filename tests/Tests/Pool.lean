/-
Copyright (c) 2026 Paul Butcher. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Postgres
import PostgresTest.Framework
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
