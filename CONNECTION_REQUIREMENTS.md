# Requirements: recovering from unusable pooled connections

Requirements only. Nothing here prescribes a design; every item states observable behaviour that
must hold afterwards. Where a requirement could be met either inside the library or by
configuration the caller supplies, that is called out as the implementer's choice.

Observations below were made against leanpostgres v0.4.1. Verify they still hold before relying
on them.

## Why this is needed

An application holding a Postgres connection open across an idle period can find that connection
unusable through no fault of its own. Two deployments make this routine rather than rare:

- **A managed database that restarts.** RDS failover, a maintenance window, or a server-side
  `idle_session_timeout` all close connections underneath a client that is otherwise healthy.
- **A process that is suspended rather than merely idle.** On AWS Lambda the execution
  environment is frozen between invocations, so the process cannot respond to anything, and the
  network path in front of it drops idle TCP flows after a few minutes. On the next invocation the
  application resumes holding a connection that no longer works.

Today an application in either situation serves an error, and the second case can present as a
request that hangs rather than one that fails. The driving consumer is a Lean web application on
Lambda, but nothing about the requirement is Lambda-specific: any long-lived process whose
database restarts hits the same thing.

## Relevant facts about the current code

These are stated so they need not be rediscovered. They are not instructions.

- `Postgres.Pool.create` opens every connection eagerly and fails the whole call if any open
  fails. `Pool.withConn` and `Pool.withConnAsync` hand a connection to the caller without
  establishing that it is still usable.
- Consequently, after any event that invalidates connections, **every** connection in the pool is
  invalid at the same time.
- `Postgres.Error` carries a five-character SQLSTATE, and `Postgres/Error.lean` documents it as
  empty for failures occurring before a result object exists, populated once the server has
  processed a statement. `Error.ofIOError?` recovers it from an `IO.Error`. This is the signal
  currently available for telling a connection-level failure from a statement the server rejected.
- `Error.ofIOError?` matches any `IO.userError` whose message starts with `[` and contains `] `,
  so it identifies the shape of an error, not its origin.
- `Postgres/FFI.lean` exposes no equivalent of `PQstatus`, `PQreset`, or `PQping`. Whether to add
  any of them is the implementer's decision.
- `Postgres.Stmt` is client-side only: `Postgres/LowLevel.lean` states that `prepare` never
  round-trips and that no server-side prepared-statement object exists. A `Stmt` does, however,
  capture the `Conn` it was created from.
- `Postgres/Pool.lean` already documents that libpq connections are unsafe for concurrent use,
  which is why the pool exists at all.
- libpq's default notice handler prints server notices, warnings, and asynchronous FATAL messages
  to stderr. The library leaves it in place except while checking whether a connection is still
  live, so ordinary notices still reach stderr uncontrolled.

## Failure modes that must both be covered

**Mode A: the connection is closed.** A FIN or RST arrives. libpq can observe this, and the next
use fails promptly.

**Mode B: the flow is silently dropped.** No FIN, no RST; the socket still appears open. The next
use blocks on TCP retransmission, which can exceed any sensible request deadline. A fix that
addresses only mode A will pass a mode-A test and still hang in production, so the two must be
distinguished when reporting what has been achieved.

## Requirements

Everything here concerns connections obtained from a pool.

**R1: A connection handed to a caller must be usable.** A caller that receives a connection from
the pool and immediately issues a valid statement must not fail because that connection had
become unusable while it sat in the pool.

This holds to the extent the connection's unusability is detectable at borrow within the bound
from R5. A connection that fails in a way indistinguishable from a healthy one at the moment it is
handed over is outside this requirement, and falls to R3 instead. Stated without that
qualification, R1 promises something no implementation can deliver, and so could never honestly be
claimed as met.

**R2: Recovery must be automatic.** Meeting R1 must not require the application to write
detection or retry logic, nor to know which failures are recoverable. An application that only
ever calls `withConn`/`withConnAsync` and issues ordinary statements must benefit.

**R3: A statement that may already have taken effect must never be re-executed.** This is the
requirement most likely to be violated by an obvious implementation. "The connection was unusable
before the statement was sent" and "the statement was sent, and the connection failed before its
result was read" are different situations. The first may be recovered from transparently. The
second must surface as an error to the caller, because silently repeating it can apply a write
twice. If the two cannot be distinguished in some case, that case must be treated as the second.

In practice the two are not distinguishable: nothing reports whether a failed statement reached
the server. The fallback clause therefore governs everywhere, and the consequence is structural
rather than cautionary. No statement issued by a caller is ever re-executed, and the only point at
which recovery may occur transparently is before the caller's first statement within a borrow.
Statements the library issues on its own behalf to establish whether a connection works are
exempt, having no effect to apply twice.

**R4: Server-side errors must not be mistaken for connection failures.** A statement the server
processed and rejected (a constraint violation, a syntax error, a permissions failure) must not
trigger recovery or retry, and must reach the caller unchanged, with its SQLSTATE intact.

This applies to an error propagating through the library's own error handling, not only to one
returned directly. In particular, an error must not lose its SQLSTATE because a cleanup step that
runs after it, such as a transaction rollback, also fails. A rollback failing is the
characteristic symptom of the connection loss this document addresses, so the current behaviour
discards precisely the diagnostics most needed. See `transaction` in `Postgres/LowLevel.lean`.

**R5: The time to discover an unusable connection must be bounded and application-controllable.**
Mode B must not be able to stall a caller indefinitely. The bound must be settable by the
application rather than fixed by the library, because the appropriate value is a property of the
deployment: a function separated from its database by a NAT that drops idle flows needs a
different value from a server sharing a host with its database. Whether this is satisfied by
something the library does, by connection parameters the application passes through, or by
documenting an existing mechanism, is the implementer's choice; but if it is satisfied by
configuration alone, that must be stated explicitly so consumers know they have to set it.

**R6: Recovery must not silently substitute a connection mid-action.** Within a single
`withConn`/`withConnAsync` action, everything the caller does must target one live connection.
Handles derived from a connection, `Stmt` in particular, capture it, so replacing a connection
part-way through an action would leave a caller holding a handle that looks valid and is not.

Nothing prevents a caller retaining a `Conn` or a `Stmt` beyond the action's extent, and this
requirement does not reach that far; a handle used after its borrow has ended is the caller's
responsibility. A `Conn` obtained directly rather than from a pool is likewise outside scope.

**R7: Failure to recover must be reported clearly.** When the database is genuinely unreachable,
the caller must receive a distinguishable error within the bound from R5. It must not hang, spin,
or retry without limit.

**R8: Existing callers must keep working.** Code calling `Pool.create`, `Pool.withConn`, and
`Pool.withConnAsync` today should continue to compile and behave as before, aside from now
recovering. If the capability cannot be delivered without an API change, say so and describe the
change rather than working around it.

**R9 (should): The cost of recovery, and of creating a pool, should scale with connections
actually used**, not with pool size. A pool of eight should not have to establish eight
connections before serving one request, whether at creation or after a failure.

**R10 (should): Recovery must be observable.** An application must be able to distinguish a
connection having been replaced from an attempt to replace one having failed, and to read both
without issuing its own probe against the database and without parsing log output. A database
flapping every few seconds must not be indistinguishable from a healthy one.

**R11: Detection must not cost a round trip on the healthy path.** Establishing that a connection
is usable must add no round trip to the server for a connection that has been in the pool less
than an application-settable interval. Where a round trip is used, it is bounded by R5. Without
this, an implementation that probes the server before every borrow satisfies every other
requirement here while adding latency to every request of an application whose database is next
door, which is a regression for the majority deployment in service of the minority one.

The interval must carry its unit in its type rather than by convention at the call site, and
disabling the probe entirely must be expressible and distinguishable from leaving the interval
unset.

How long a connection has been idle must never be under-reported. A measurement that reads as no
time at all because the process was suspended, or because the clock was adjusted underneath it,
satisfies R11 while defeating R1, silently and in exactly the deployment that motivates both. The
two errors are not symmetric: over-reporting idle time costs one unnecessary probe, and
under-reporting it costs a caller a connection that does not work.

**R12: The pool's capacity must be invariant.** No path, including one where recovery is attempted
and fails, may consume pool capacity without returning it. A pool that has failed to recover any
number of times must still serve `size` concurrent callers once the database is reachable again.
The failure this prevents is a pool that sheds a slot per failed recovery, empties during an
outage, and then blocks every subsequent borrow indefinitely, long after the database itself has
recovered.

**R13: A connection known to be unusable must not be handed to another caller as though it were
healthy.** Whether this is achieved when the connection is returned, when it is next borrowed, or
both, is the implementer's choice. The requirement is deliberately about connections *known* to be
unusable: an exception raised by a caller's action cannot in general be attributed to the
connection rather than to the caller's own code, so an implementation may defer discovery to the
next borrow. What it may not do is leave a slot permanently occupied by a connection that fails
every time it is used.

**R14: A database that is unreachable when the pool is created must not permanently disable the
application.** Two situations must be handled differently. A configuration that can never work,
such as unknown credentials or an unresolvable host, may reasonably fail creation, and failing a
deploy on it is worth preserving. A database that is merely unreachable at that moment must not,
because the events this document exists to address cause cold starts as well as arriving during
them: a failover errors in-flight work, traffic retries, new processes start, and each one creates
its pool against a database that is still failing over. Nothing available reliably separates the
two cases, so the choice must be exposed to the application rather than inferred from an error
message.

Creating a pool defaults to requiring a connection. That keeps a connection string that can never
work failing at startup, where it is cheap to notice, and it is the behaviour R8 preserves for
existing callers. An application that must survive creation against a database that is merely away
opts out explicitly.

**R15 (should): A replaced connection is a new session, and the application must be able to say
what that session needs.** Session-scoped state does not survive replacement: `SET` parameters, a
runtime `search_path`, temporary tables, `LISTEN` registrations and advisory locks are all gone.
That no such state survives a borrow must be stated explicitly, since an application that sets it
in one borrow and depends on it in the next fails silently and intermittently rather than
obviously. An application should additionally have some way to run its own initialisation against
a newly opened connection, so that a replacement is equivalent to its predecessor rather than
merely functional.

## Non-goals

- No general retry, backoff, or circuit-breaker framework.
- No change to how queries, codecs, or results work.
- No read-replica awareness, failover orchestration, or multi-host connection strings.
- Fixing applications that share a single `Conn` across concurrent fibers is out of scope; the
  pool already exists for that and is already documented as the answer.
- Coordinating recovery between concurrent callers. During an outage every caller attempts to
  re-establish its own connection independently, with no shared backoff, damping or admission
  control. The consequence is that a pool of eight facing an unreachable database costs eight
  independent connection attempts, each bounded only by R5. This is a deliberate consequence of
  ruling out a circuit breaker, recorded here so it is not discovered later.
- Classifying a connection failure as permanent or transient by inspecting the message text libpq
  produces. Those messages are translated when NLS is enabled and vary between versions.
- Connections obtained outside a pool.

## What must be demonstrated

Each of these should fail before the change and pass after, except where noted.

1. A connection that has been closed server-side while sitting in the pool is nevertheless usable
   when next borrowed. Terminating the backend from a second session is enough to set this up, but
   the test must let the server's message and end of file actually arrive before borrowing, which
   phase 0 measured at up to a few milliseconds. That settling period is network delivery, not a
   TCP timeout, so it costs milliseconds rather than the minutes a retransmission would; it should
   be waited for by retrying within a generous bound rather than by a fixed sleep.
2. A statement the server rejects (for example a unique-constraint violation) reaches the caller
   with its SQLSTATE, and no recovery is attempted.
3. A write is not applied twice when a connection fails around it. This is not provoked by a test.
   The argument relied on instead is that the library contains no path that issues a caller's
   statement a second time: there is no retry anywhere, and recovery is confined to the point
   before a borrow's first statement, so a repeat would have to be introduced deliberately rather
   than merely failed to be prevented. This is a standing obligation on every later change, not a
   property established once.
4. A borrow that cannot be satisfied because the database is unreachable returns an error within
   a bounded time rather than hanging.
5. Mode B, a silently dropped flow. If this cannot be exercised in the library's test
   environment, say so explicitly and describe what a consumer must configure to be protected,
   rather than leaving it implied that mode A coverage covers both.
6. Capacity survives a failed recovery. With the database stopped, exhaust every connection in the
   pool and collect the resulting errors; then start the database and show that `size` concurrent
   callers are all served. This is the test that catches a pool which shrinks silently, whose
   symptom otherwise appears long after the outage and looks nothing like its cause.
7. An exception raised by a caller's own action, unrelated to the connection, does not cause a
   replacement. Observable through R10.
8. A pool created while the database is unreachable, in the configuration where that is permitted,
   yields a pool whose first borrow succeeds once the database returns.
9. An error raised inside a transaction on a connection that then fails reaches the caller with
   its SQLSTATE intact, rather than flattened into the message of a rollback failure.
10. A borrow from a pool in steady use issues no additional statement to the server. If this
    cannot be measured without depending on server-side statistics views, say so and record what
    is relied on instead.

The existing suite already runs against a real Postgres, so tests needing server-side effects are
feasible. Any test that cannot be made deterministic should be left out and its absence recorded,
in preference to a timing-dependent test.

## Answers

- **Is the empty-SQLSTATE discriminator sufficient in practice?** No. It is necessary but not
  sufficient, and not only for the obvious reason: several errors the library raises itself, such
  as a parameter index out of range or a missing current row, carry no SQLSTATE at all, so
  treating absence as evidence of connection failure misclassifies caller bugs as recoverable.
  Meeting R3 and R4 requires positive confirmation of connection state, which requires exposing
  more of libpq than `Postgres/FFI.lean` does today.
- **Does R5 end up satisfied inside the library, or as an obligation on consumers?** On consumers,
  and it already works. Connection strings are passed to libpq unmodified, so `connect_timeout`,
  `tcp_user_timeout` and the `keepalives_*` family are available now, and they are the only things
  that bound mode B. The obligation is to document this as a requirement on consumers rather than
  to imply the library handles it.
- **Does anything force a change to `Pool`'s eager-open behaviour?** R1 through R13 do not. R14
  does, and R9 as amended does.
- **What unit and type does the idle interval in R11 take?** A `Std.Time.Duration`, wrapped so
  that "never probe" remains expressible. The library already exposes `Std.Time` types in its
  public API, so this adds no dependency, and a duration that carries its own unit avoids the trap
  libpq itself falls into, where `connect_timeout` is measured in seconds and `tcp_user_timeout` in
  milliseconds.
- **Which clock measures the idle interval in R11?** Both, taking the greater of the two elapsed
  times. `Std.Time` offers no monotonic clock, and `IO.monoMsNow` is backed by `CLOCK_MONOTONIC`,
  which on Linux excludes time the process spent suspended; a wall clock on its own can be stepped
  by an adjustment. Neither alone can be relied on to satisfy R11's under-reporting rule, and
  because the two error directions are not symmetric, the maximum is safe in the direction that
  matters. This also settles the question without first having to establish empirically how a
  Lambda freeze affects the guest's clocksource.

## Order of work

Not requirements. Everything above states what must hold; this records one workable order for
getting there, and what each step discharges, so that partial progress can be assessed against the
requirements rather than by inspection. Phases are ordered by dependency, not by priority.

**Phase 0: establish that detection is free.** Confirm that a connection whose backend has been
terminated from a second session can be recognised as unusable without a round trip to the server,
and measure how often that check is reached before the server's FIN has arrived. Phases 2 and 4
assume this is both cheap and reliable. If it is not, R1 and R11 have to be reconciled some other
way and the rest of this order changes shape, which is why it comes first. Discharges nothing;
de-risks R1, R11 and R13.

*Done.* The check is free on the healthy path and reliable in production, but it is neither a
single call nor instantaneous. Establishing liveness costs around 0.2 microseconds and no round
trip on a healthy idle connection, with no false report of death in 100000 calls and the connection
still usable afterwards. Detecting a terminated backend, however, means draining libpq's input
until it reports end of file: the first read consumes the server's FATAL message and reports
success, and only the read after it reports the connection gone, so any check that looks once
always concludes the connection is healthy. Delivery is not immediate either. Against a server one
Docker bridge away, detection succeeded 0 times in 100 immediately after `pg_terminate_backend`
returned, 67 at one millisecond, 92 at five, and 100 at fifty. Two consequences for later phases:
the drain loop belongs in C, where it needs no termination argument, exposed to Lean as a single
call; and a notice handler must be installed, or the FATAL that the drain consumes is printed to
stderr by libpq on the library's behalf.

**Phase 1: error fidelity.** Record that no statement issued by a caller is ever re-executed, and
stop `transaction` flattening an error's SQLSTATE into the message of a failed rollback. This
depends on nothing else here and can land on its own. It also fixes a defect that exists today,
independently of whether any recovery work follows. Discharges R3 and R4, and demonstrations 3
and 9.

*Done.* `transaction` now rethrows the error that ended the action rather than a fresh one
describing the cleanup, so its SQLSTATE survives. Demonstration 9 was checked against the previous
behaviour and does fail there, with the original code flattened to
`Rollback failed: [] server closed the connection unexpectedly`; without that check it would have
passed either way, since an error whose rollback succeeds takes a different path to the same
place. One deviation from R4 as written: the rollback's own failure is appended to the message,
leaving the SQLSTATE recoverable but the message not strictly unchanged. Dropping it entirely
would have left a caller holding a connection with an open transaction and no indication of it.

**Phase 2: connection liveness in the FFI.** Expose enough of libpq to answer whether a connection
is still usable without issuing a statement against it. Enables R1 and R13 but discharges neither
on its own.

*Done.* `Conn.isLive` answers whether a connection is still usable without sending anything to the
server, at roughly 0.2 microseconds and no round trip when the connection is healthy. The drain
loop sits in C, as phase 0 concluded, so nothing in Lean needs a termination argument for it.
Notices are suppressed for the duration of the drain and restored afterwards rather than
permanently: ordinary `NOTICE` output still reaches stderr exactly as before, while a check that
happens to consume a dying connection's parting message no longer prints it on the application's
behalf.

That one check suffices is pinned by a test of its own, confirmed to fail against an implementation
that reads once. It earns its place: the test that merely retries until the connection is reported
closed passes either way, so without it a regression to a single read would leave every borrow
handing out a dead connection with the suite still green.

**Phase 3: pool capacity and deferred filling.** Separate pool capacity from the number of
connections currently established, so that a unit of capacity can be empty rather than always
holding a connection, and so that every path out of a borrow returns exactly one unit whether or
not it succeeded. Creation then establishes one connection rather than `size`. Discharges R12, R14
and R9, and demonstrations 6 and 8. The concurrency bound the existing pool tests already check
must be unaffected.

*Done.* Capacity and connections are now separate things. A channel holds `size` permits, which is
what bounds concurrency and what a caller waits on; the connections sit in a stack of the ones
nobody is using. Every path out of a borrow releases exactly one permit. Creation opens one
connection instead of `size`, and `PoolOptions.requireConnection` carries R14's choice, defaulting
as decided. Existing call sites are untouched.

R12 is checked by a test that was confirmed to catch a pool dropping a unit of capacity per failed
open, reporting it as such. That test has an unavoidable wart, recorded alongside it: when it
fails, the run hangs afterwards, because establishing that borrows no longer block requires
borrows that would block if they did, and a blocked borrow keeps the process alive. The failure is
reported before the hang.

R9 is met in both halves. Cost scales with concurrent demand rather than capacity: a pool of eight
serving one caller at a time opens one connection, not eight. Two separate things deliver that, and
conflating them cost a wrong claim in a doc comment before a test caught it. Separating permits
from connections is what stops sequential borrows walking through the whole capacity. Taking the
most recently returned connection rather than the least is what makes a pool settle back onto one
after a burst has forced it to open several, instead of keeping all of them in rotation
permanently. The first is what R9 asks for; the second decides whether a pool ever recovers from
its own high-water mark, which R9 does not ask about and which matters just as much to a server
counting connections.

Demonstrations 6 and 8 are each half exercised. The half that matters, that capacity is not lost
and the pool does not deadlock, is tested. The half that needs the database to stop and then start
is not, because the suite runs against a server it does not control. What is relied on instead:
a unit of capacity returned after a failed open is `vacant`, indistinguishable from one that has
never been filled, so the first successful open after an outage takes the same path as the first
open of a fresh pool, which is covered.

**Phase 4: replacement at borrow.** Apply the phase 2 check when a connection is taken from the
pool, and route a connection that fails it into the phase 3 filling path, so that filling an empty
unit of capacity and replacing a dead connection are the same operation rather than two. Discharges
R2, R6 and R13, and R1 for mode A, and demonstrations 1 and 2.

*Done.* Taking a connection from the pool checks it first, and one that fails is dropped rather
than handed over, which leaves the permit holding no connection and so falls into the same path
that opens one for a permit that never had one. The two cases are one line, not two mechanisms.
Demonstration 1 was confirmed to fail without the check, with the error an application would
actually serve: `[] server closed the connection unexpectedly`.

R6 holds by construction rather than by care: the only point at which a connection is replaced is
before the caller's action begins, so there is no moment at which a `Stmt` could be left addressing
a connection the pool has since swapped out.

Demonstration 2 is covered by a test that checks both halves, since the SQLSTATE surviving is only
half the requirement. It asserts that a rejected statement reaches the caller as `22012` *and* that
the pool still holds the same backend afterwards, which is what an implementation treating every
error as a connection failure would break. Demonstration 7 arrived early and for free: comparing
backend process ids before and after establishes that a caller's own exception replaces nothing,
without needing the counters R10 will bring.

**Phase 5: the idle threshold.** Add the interval R11 describes, measured as the greater of the two
clocks, and a round-trip check for connections idle beyond it. Discharges R11, and R1 for mode B to
the extent the consumer has configured R5. Demonstrations 4, 5 and 10 attach here, as does the
consumer obligation R5 carries, which must be written down rather than implied.

*Done.* `PoolOptions.validateAfterIdle` is a `Std.Time.Duration`, defaulting to 30 seconds, with
`none` meaning never. Idle time is the greater of the monotonic and wall-clock elapsed times, as
decided. A connection past the threshold is sent a statement, and anything other than a plain
answer replaces it; that also disposes of a connection left in an aborted transaction, which is
live and reachable and would fail every statement until someone rolled it back, and which nothing
before this phase would have caught.

Demonstration 10 turned out to be testable after all, rather than needing server-side statistics
as feared: asking the server what it last saw on a backend establishes what the pool sent without
sending anything to find out. It and the threshold check form a matched pair, each confirmed to
fail if the other's behaviour is substituted, so neither can pass vacuously.

Demonstration 4 is covered by the borrow against an unreachable database in the creation test. The
bound on it is `connect_timeout`, which is the consumer's to set.

Demonstration 5, mode B, is **not exercised**, and cannot be in this suite: provoking a silently
dropped flow needs a middlebox that discards packets without resetting, which a test running
against a real Postgres over a loopback or bridge has no way to arrange. What stands in its place
is the check this phase adds plus a documented obligation. The library cannot bound how long that
check takes to fail; `tcp_user_timeout` and `keepalives_*` in the caller's own connection string
are what bound it. That obligation is now stated in the README under its own heading, in
`PoolOptions.validateAfterIdle`, and on `Pool.withConn`, rather than left to be inferred from mode
A working.

**Phase 6: observability and the options surface.** The counters R10 requires, and the options
carrying the creation choice from phase 3 and the interval from phase 5, defaulted so that existing
call sites are unaffected. Discharges R10, confirms R8, and discharges demonstration 7.

*Done.* `Pool.statistics` returns counts of connections opened, connections discarded as unusable,
and opens that failed. Those three separate the two states R10 asks to be told apart: a replacement
that worked raises opened and discarded together, one that failed raises discarded and openFailures
together, and a borrow with nothing to discard that still couldn't open raises openFailures alone.
A borrow that replaced nothing moves none of them, which the test asserts explicitly, since
counters that rise on every borrow would say no more than no counters at all.

Most of this phase was already standing. The options surface arrived with the requirements it
serves, in phases 3 and 5, and demonstration 7 in phase 4, where comparing backend process ids
turned out to answer it without needing counters at all.

R8 is confirmed by the suite: every `Pool.create` call written before any of this work compiles and
passes unchanged, including the concurrency-bound and release-on-throw tests that predate the
requirements document.

**Phase 7 (should): session initialisation.** A way for an application to prepare a newly opened
connection, so that a replacement is equivalent to its predecessor rather than merely functional.
Discharges R15. Independent of phases 4 to 6 and can be deferred without affecting them.

*Done.* R15's `must` is met: `Pool.withConn` and the README both now state that
nothing set on a session survives past the end of an action, name what that covers, and say where
such state has to go instead. That half is the one that stops the bug being written, since setting
something in one borrow and depending on it in a later one appears to work until a connection is
replaced or a borrow lands elsewhere.

The documentation is specific about which answer applies to which kind of state, because they
differ and one instruction covering all of them is wrong for most. Server settings go in
`conninfo`, where the server applies them to every connection including replacements, at no cost.
Temporary tables and advisory locks go inside the action, and a session-scoped advisory lock held
across borrows is broken whether or not anything is ever replaced. `LISTEN` cannot be pooled at
all, since notifications reach only the connection that registered them and only while it is
borrowed, so it needs a connection kept outside the pool.

A `Conn` still carries no identity an application can compare against one it held before, which is
why the signal has to come from the pool: nothing an application can do from outside would tell it
whether the connection it has is one it has already prepared.

R15's `should` is met, by telling the application rather than by acting for it. `Pool.withBorrowed`
hands over a `Borrowed`, carrying the connection and whether the session has been set up yet, so an
application establishes its own state in its own code, once per connection instead of once per
borrow.

A hook that ran the application's setup for it was considered first and rejected in favour of this.
Every hazard the hook carried came from the pool running the application's code: a hook that
borrows from the pool it is initialising deadlocks; a hook runs at times the application does not
choose; a throwing hook is indistinguishable from a database outage in the counters. Reporting
carries none of these, because the setup is then ordinary code in the place the application wrote
it. It is also more expressive than either hook shape considered, since it is not a callback at
all.

Two details are load-bearing, and both were confirmed by removing them and watching a test fail.
A connection replaced part-way through a pool's life reports a new session, without which an
application relying on session state meets `42P01 relation does not exist` after a failover. And
the flag clears when an action completes rather than when the connection is handed over, because an
action that threw may have thrown during its own setup, and there is no way to tell from outside;
redoing work that may already have been done is the recoverable error, relying on work that was
never done is not. The cost is that setup must be safe to repeat, which is stated where the
application will read it.
