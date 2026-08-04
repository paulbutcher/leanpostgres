/-
Copyright (c) 2026 Paul Butcher. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module
import all Postgres.FFI
public import Postgres.FFI
public import Std.Async.Basic
import Std.Sync.Channel
import Std.Sync.Mutex

set_option doc.verso true
set_option linter.missingDocs true

namespace Postgres.Async

public section

open Std (CloseableChannel Mutex)
open Std.Async (Async)

/--
A single pending "tell me when this fd is ready" request submitted to the background poller
(see `waitSocket`).
-/
private structure PollRequest where
  fd : Int32
  wantWrite : Bool
  promise : IO.Promise (Except IO.Error Unit)

/--
Set by `shutdown` to tell the poller loop to stop recursing and let its task finish, rather than
run forever. Lean's runtime waits for every live task (including `Task.Priority.dedicated` ones)
before a process can actually exit, so without this, any program that ever calls into this module
would hang on exit instead of terminating; see `ASYNC_DECISIONS.md`.
-/
private initialize shutdownRequested : IO.Ref Bool ← IO.mkRef false

/--
One iteration's worth of driving `FFI.poll`: drain any newly-submitted requests from `requests`
without blocking, then block in `FFI.poll` until something changes (a watched fd, a newly
registered wait, or `shutdown`, all of which reach here via `FFI.wake`), resolving whichever
requests are now ready. Always includes `requests` in the poll set (via a plain, non-blocking
`tryRecv` drain rather than `CloseableChannel`'s own blocking `recv`) so `FFI.wake` is the *only*
mechanism ever needed to interrupt this loop, whether it's idle or has fds to watch; that in turn
is what lets `shutdown` reliably wake it regardless of what it's currently doing. `working` is this
loop's own local state; nothing outside this function ever touches it, so it doesn't need the same
cross-thread protection `requests` does.
-/
private partial def pollerStep (requests : CloseableChannel PollRequest) (working : Array PollRequest) :
    IO (Array PollRequest) := do
  let rec drain (acc : Array PollRequest) : IO (Array PollRequest) := do
    match ← requests.tryRecv with
    | some req => drain (acc.push req)
    | none => return acc
  let working ← drain working

  let fds := working.map (·.fd)
  let wantWrite := working.map (·.wantWrite)
  match ← (FFI.poll fds wantWrite).toBaseIO with
  | .error e =>
    -- A fatal `poll()` failure (not an individual fd erroring, which is reported as "ready" so
    -- the caller's own libpq call surfaces the real error): fail every pending request instead
    -- of leaving them to hang forever.
    for req in working do
      req.promise.resolve (.error e)
    return #[]
  | .ok ready =>
    -- `drain` above may have added requests after `fds`/`wantWrite` were snapshotted for this
    -- `FFI.poll` call; anything past `ready`'s length just hasn't been polled yet this iteration.
    let mut remaining : Array PollRequest := #[]
    for h : i in [0:working.size] do
      if h' : i < ready.size then
        if ready[i] then
          working[i].promise.resolve (.ok ())
        else
          remaining := remaining.push working[i]
      else
        remaining := remaining.push working[i]
    return remaining

/--
The poller loop body. Wrapped in `try`/`catch` so a genuinely unexpected exception (as opposed to
the handled `poll()`-failure case in `pollerStep`) surfaces as an error to whoever is currently
waiting rather than silently deadlocking them; there's no supervisor restarting this task, by
design (see `ASYNC_DECISIONS.md`), so bugs here are meant to be loud rather than papered over.
Stops (rather than recursing again) once `shutdownRequested` is set, failing anything still pending
at that point.
-/
private partial def pollerLoop (requests : CloseableChannel PollRequest) (working : Array PollRequest) :
    IO Unit := do
  let working' ←
    try
      pollerStep requests working
    catch e =>
      for req in working do
        req.promise.resolve (.error e)
      pure #[]
  if ← shutdownRequested.get then
    for req in working' do
      req.promise.resolve (.error (.userError "Postgres.Async poller shut down"))
  else
    pollerLoop requests working'

/-- The channel and background task backing every `waitSocket` wait; see `getPoller`. -/
private structure PollerState where
  requests : CloseableChannel PollRequest
  task : Task (Except IO.Error Unit)

/--
Guards lazy, at-most-once creation of the poller. Deliberately *not* started eagerly at module load
(the first version of this file did so via `initialize pollerTask ← IO.asTask ...`): that was found
to hang plain *compilation* of any downstream file that merely imports this module, not just
programs that actually call `waitSocket` (see `ASYNC_DECISIONS.md` for how this was diagnosed).
Starting the poller lazily, on the first genuine runtime call, means mere compilation never touches
this code path at all. Only the inert mutex itself is constructed eagerly here, which is not what
caused the hang.
-/
private initialize pollerGuard : Mutex (Option PollerState) ← Mutex.new none

/--
Returns the shared poller, creating it (and spawning its dedicated background task) on the first
call. `Task.Priority.dedicated` gives it a real OS thread of its own rather than the default shared
worker-pool priority: this loop spends most of its life blocked (in `FFI.poll` or waiting on the
channel), and blocking a shared-pool worker thread on work that can only be produced by *another*
task on that same bounded pool is a starvation deadlock waiting to happen, not just a performance
concern (confirmed by reproducing the hang directly; see `ASYNC_DECISIONS.md`).
-/
private def getPoller : IO PollerState :=
  pollerGuard.atomically do
    match ← get with
    | some state => return state
    | none =>
      let requests ← CloseableChannel.new
      let task ← IO.asTask (pollerLoop requests #[]) Task.Priority.dedicated
      let state := { requests, task }
      set (some state)
      return state

/--
Resolves once `fd` becomes ready for reading (`wantWrite = false`) or writing (`wantWrite = true`).
Doesn't block the calling thread: the wait is driven by a single dedicated background task shared
across every caller, so this is safe to call from many concurrent `Async` computations at once.
-/
def waitSocket (fd : Int32) (wantWrite : Bool) : IO (IO.Promise (Except IO.Error Unit)) := do
  let poller ← getPoller
  let promise ← IO.Promise.new
  discard <| poller.requests.trySend { fd, wantWrite, promise }
  FFI.wake
  return promise

/--
`Async`-native version of `waitSocket`, for use directly inside an `Async` computation; unlike
`waitSocket` itself, a poller-reported error surfaces as this `Async` computation's own failure
rather than as an `Except` value the caller must unwrap.
-/
def waitSocketAsync (fd : Int32) (wantWrite : Bool) : Async Unit :=
  Async.ofPromise (waitSocket fd wantWrite)

/--
Stops the background poller if one was ever started (a no-op if `waitSocket`/`stepAsync` was never
called), and blocks until it has actually finished. **Must be called before your program exits if
it ever used any of this module's async functionality** (directly, or via `Stmt.stepAsync`/
`Stmt.execAsync`): Lean's runtime waits for every live task, including the poller's dedicated one,
before a process can exit, and the poller otherwise runs forever, so without calling this first,
the process hangs on exit rather than terminating. This is a real, somewhat awkward constraint of
Lean's task model for a background-service-shaped task like this one, not a preference; see
`ASYNC_DECISIONS.md`.
-/
def shutdown : IO Unit := do
  let poller? ← pollerGuard.atomically get
  match poller? with
  | none => return ()
  | some poller =>
    shutdownRequested.set true
    FFI.wake
    match poller.task.get with
    | .ok () => pure ()
    | .error e => throw e

end
end Postgres.Async
