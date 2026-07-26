# Project plan: leanpostgres implementation

Implementation plan for the library designed in
[`postgres-port-design.md`](postgres-port-design.md). Read that
document first — this plan sequences its architecture into milestones
with tasks, exit criteria, dependencies, and a risk register. It
doesn't re-derive design decisions already made there.

## Assumptions

- One engineer, full-time, with working knowledge of Lean FFI (the
  `leansqlite` codebase is the reference for conventions — `extern`
  declarations, `lean_external_class`, `lean_obj_res`/`b_lean_obj_arg`
  argument ownership rules, Verso doc comments).
- Tests are written alongside each milestone, not deferred to a single
  "testing phase" at the end — this is a deviation from the design
  doc's three-phase summary (Core / Full catalog / Hardening), which
  compressed testing into phase 3 for estimation purposes only. Here,
  each milestone's exit criteria include its own tests, and a final
  milestone handles cross-cutting integration/CI concerns rather than
  first-time test authoring.
- A local and CI Postgres instance are available from milestone 0
  onward (see M0) — the plan does not defer "get a real database
  running" to late in the schedule, since every milestone after M1
  needs one to test against.
- Total: **~40 working days (8 weeks) nominal, 7–10 weeks with
  contingency**, consistent with the design doc's estimate. A two-
  engineer split that parallelizes the extended type catalog (M7)
  and result-metadata work (M8) against the core path could compress
  this to roughly 5–6 weeks wall-clock; noted per-milestone below.

## Milestone summary

| # | Milestone | Days | Depends on | Parallelizable with |
|---|---|---|---|---|
| M0 | Project scaffolding & build | 3.5 | — | — |
| M1 | Connection layer | 3 | M0 | — |
| M2 | Statement buffering & execution | 5 | M1 | — |
| M3 | Core type catalog + Row/QueryIterator | 4 | M2 | — |
| M4 | Interpolation macros | 2 | M3 | M5 |
| M5 | Transactions & error semantics | 3 | M2 | M4 |
| M6 | Deriving handlers | 5 | M3 | M7, M8 |
| M7 | Extended type catalog | 6 | M3 | M6, M8 |
| M8 | Result metadata & remaining LowLevel surface | 2.5 | M2 | M6, M7 |
| M9 | Test suite consolidation & CI hardening | 3.5 | M4–M8 | — |
| M10 | Documentation & release polish | 2.5 | M9 | — |

M0–M3 are a strict critical path (build → connect → execute → bind/read
types); nothing else can start before M3 lands, since every later
milestone needs working `QueryParam`/`ResultColumn`. After M3, M4–M8
only depend on M3 (and M2/M5 where noted) and can run in any order —
this is the fan-out point for a second engineer.

## M0 — Project scaffolding & build (3.5 days)

- Create the `leanpostgres` package: `lakefile.lean`, `lean-toolchain`
  (match the current `leansqlite` toolchain version), `LICENSE`
  (Apache-2.0, matching), root `Postgres.lean` re-export skeleton.
- `bindings/leanpostgres.c` skeleton: `leanpostgres_initialize`
  registering `Conn`/`Result` external classes (finalizers can be
  stubs initially), mirroring `leansqlite_initialize` in
  `bindings/leansqlite.c:106-115`.
- `lakefile.lean` `extern_lib` target: locate libpq via `pkg-config
  --cflags --libs libpq`; add an environment-variable override
  (`LEANPOSTGRES_PQ_INCLUDE`/`LEANPOSTGRES_PQ_LIB` or similar) for
  platforms without `pkg-config`.
- `.devcontainer/Dockerfile`: add `libpq-dev`. Add a Postgres service
  (devcontainer feature or a `docker-compose.yml` alongside it) so
  `lake build`/`lake test` can hit a live database from inside the
  container without extra setup.
- CI skeleton: new GitHub Actions workflow (or extend
  `.github/workflows/lean_action_ci.yml`) with a `services: postgres:`
  block; a trivial smoke job that runs `lake build`.
- **Exit criteria**: `lake build` succeeds and links against libpq on
  at least Linux (CI) and one other platform you develop on (macOS
  likely, given `Homebrew libpq` is mentioned in the design doc);
  `leanpostgres_initialize` round-trips from a `#eval`/test call; CI
  is green on the empty build.

## M1 — Connection layer (3 days)

- `FFI.Conn` external class with a `PQfinish` finalizer.
- `leanpostgres_open(conninfo)`: `PQconnectdb` + `PQstatus` check;
  error path reads `PQerrorMessage` (matching `leansqlite_open`'s
  shape in `bindings/leansqlite.c:118-130`).
- `Postgres.Error` structure (`{ sqlstate : String, message : String
  }`) and the error-construction path from `PQresultErrorField`/
  `PQerrorMessage` — build this now even though no query has run yet,
  since M2 depends on it existing.
- `Postgres.open (conninfo : String) : IO Conn` in `LowLevel.lean`.
- **Tests**: successful connect against the CI/dev Postgres instance;
  connect failure (bad conninfo, unreachable host) surfaces a
  `Postgres.Error` rather than a generic `IO.userError`.
- **Exit criteria**: both tests pass in CI.

## M2 — Statement buffering & execution (5 days)

The largest single milestone — it's the architectural piece that's
genuinely new relative to `leansqlite` (see the design doc's
[Preparing and executing](postgres-port-design.md#preparing-and-executing)
section).

- `FFI.Result` external class (`PQclear` finalizer); wrappers for
  `PQntuples`, `PQnfields`, `PQgetvalue`, `PQgetisnull`,
  `PQresultStatus`, `PQresultErrorField`.
- `leanpostgres_exec_params`: builds a C `char*[]` (with a matching
  `int[]` length array and NULL markers) from a Lean parameter array,
  calls `PQexecParams` with null format arrays (text mode per the
  design doc), and returns either the wrapped `FFI.Result` or a
  `Postgres.Error` depending on `PQresultStatus`
  (`PGRES_TUPLES_OK`/`PGRES_COMMAND_OK` vs. the error statuses).
- `LowLevel.Stmt`: implement the mutable-parameter-buffer design from
  the design doc. Since `Stmt` is a pure Lean record with no backing
  C object (unlike `sqlite3_stmt`), the params array and the
  post-execution `(Result, cursor)` pair need explicit mutable state
  — an `IO.Ref` held in the record is the natural choice here (there
  is no equivalent decision to make in the SQLite version, since
  `bindText` et al. mutate the C statement object directly). Decide
  and document this once; it's the one place the port's mutability
  story diverges structurally from `leansqlite`.
- `bindText`/`bindInt32`/`bindInt64`/`bindFloat`/`bindBlob`/`bindNull`
  updating the ref (encoding to text happens here or is deferred to
  M3's typeclasses — decide based on whether `LowLevel` should know
  about encoding at all; recommend deferring encoding to `QueryParam`
  instances in M3 and having `LowLevel`'s bind functions take
  pre-encoded `Option ByteArray`/`String`, matching how `leansqlite`
  keeps `LowLevel` type-agnostic).
- `Stmt.step`: first call flushes the buffered params through
  `leanpostgres_exec_params`, stores the result + cursor at 0;
  subsequent calls advance the cursor against `PQntuples`.
- `Stmt.exec` convenience wrapper (`discard stmt.step`, matching
  `LowLevel.lean:450`).
- **Tests**: parameterized `INSERT`/`SELECT` round trip; multi-row
  iteration via repeated `step`; empty result set (`step` returns
  `false` immediately); non-`SELECT` commands (`UPDATE`/`DELETE`)
  don't error despite returning no rows; a deliberately malformed
  statement surfaces a `Postgres.Error` with the right SQLSTATE
  (`42601` syntax error).
- **Exit criteria**: all of the above pass in CI.

## M3 — Core type catalog + Row/QueryIterator (4 days)

- `QueryParam`/`NullableQueryParam`/`ResultColumn`/
  `NullableResultColumn` typeclasses, ported from
  `SQLite/QueryParam.lean` and `SQLite/QueryResult.lean` — the
  `Option`-lifting instances (`QueryParam.lean:59-74`,
  `QueryResult.lean:66-80`) need no changes at all.
- Text encode/decode instances for the core set: `Bool`, `Int16`,
  `Int32`, `Int64`, `Float32`, `Float`, `String`, `ByteArray` (hex
  `\x...`), `Unit`/`NULL`.
- `RowReader`, `Row`, `Row (α × β)` instance, `QueryIterator` (via
  `Std.Iterators`), `Stmt.results`/`Stmt.resultsAs` — ported from
  `QueryResult.lean:82-168` with `columnText`-style accessors
  redirected to `PQgetvalue`/`PQgetisnull` on the buffered `Result`
  instead of `sqlite3_column_*`.
- **Tests**: round-trip every core type including `NULL`
  (`Option α`); a 3+-column table exercising the tuple `Row`
  instance; iterate a multi-row result to completion.
- **Exit criteria**: a table with a mix of the core types can be
  populated and read back through `Row`-derived tuples, matching the
  coverage `leansqlite` has for its five storage classes.

## M4 — Interpolation macros (2 days)

- Port `SQLite/Interpolation.lean`'s `sql!`/`exec!`/`query!` macros
  with the placeholder text changed from `s!"?{index}"` to
  `s!"${index}"` (`Interpolation.lean:54`) — the rest of the macro
  (chunk walking, `NullableQueryParam.bind` codegen) is unchanged.
- **Tests**: interpolated insert and query with a mix of literal SQL
  and interpolated parameters, matching the pattern
  `leansqlite`'s own tests use for `sql!`/`query!`.
- **Exit criteria**: macro-based queries compile and execute
  correctly against the M3 type catalog.

## M5 — Transactions & error semantics (3 days)

- `IsolationLevel`, `TransactionOptions`, `beginTransaction`/`commit`/
  `rollback`/`transaction` wrapper, per the design doc's
  [Transactions](postgres-port-design.md#transactions) section.
- Verify the `Postgres.Error`/SQLSTATE plumbing from M1/M2 is
  consistently populated across every execution path added since
  (M2's `exec_params`, M3's typed reads failing to parse).
- **Tests**: `BEGIN` text generation for each `TransactionOptions`
  combination; `transaction` wrapper commits on success and rolls
  back on a thrown exception; a unique-constraint violation surfaces
  SQLSTATE `23505` (assert on the code, not the message string — the
  design doc calls out message-text parsing as fragile/
  locale-dependent, so tests should model the behavior callers are
  meant to rely on).
- **Exit criteria**: all of the above pass; at least one test asserts
  on a SQLSTATE code rather than a message substring, to catch a
  future regression that silently drops the code.

## M6 — Deriving handlers (5 days)

- Port `QueryResult/Deriving.lean` and `Blob/Deriving.lean` (~650
  lines combined) plus `Blob/Classes.lean`, retargeted at the new
  `ResultColumn`/`QueryParam`/`ToBinary`/`FromBinary` instances.
  `Blob`'s binary format binds to `bytea` instead of `BLOB`.
- **Tests**: port the fixtures from `tests/SQLiteTest/Deriving.lean`
  and `tests/SQLiteTest/BlobDeriving.lean` — single-constructor
  record types with `deriving Row`/derived blob (de)serialization,
  including `Option` fields and nested wrapper types.
- **Exit criteria**: derived-instance test coverage is at parity with
  the two `leansqlite` test files it's ported from.

## M7 — Extended type catalog (6 days)

The largest source of genuinely new implementation work (no
`leansqlite` code to port from for any of these):

- `Postgres.Numeric`: wrapper around Postgres's exact decimal text
  (sign/digits/scale), plus documented lossy `toFloat`/`toInt`
  conversions. No arithmetic on `Numeric` itself in v1.
- `Postgres.Uuid`: 16-byte representation, hyphenated-hex text
  parse/format.
- `Postgres.Date`/`Time`/`Timestamp`/`Timestamptz`: ISO text
  parse/format. For `timestamptz` specifically, write tests against
  an explicit `SET TimeZone = 'UTC'` session (or equivalent) rather
  than relying on the test database's default zone — Postgres
  normalizes `timestamptz` text output to the session's `TimeZone`
  setting, so tests that don't pin it are a latent source of flaky,
  environment-dependent failures.
- 1-D arrays (`Array α` given `QueryParam α`/`ResultColumn α`):
  dedicated parser/printer for Postgres's `{a,b,c}` array literal
  grammar, including element quoting rules (elements containing `,`,
  `{`, `}`, whitespace, or the literal `NULL` token need quoting) and
  `NULL` elements within the array.
- **Tests**: round-trip each new type; array edge cases specifically
  — empty array, array containing `NULL`, array containing elements
  that require quoting.
- **Exit criteria**: every type in the design doc's [type
  catalog](postgres-port-design.md#type-catalog) table has working,
  tested `QueryParam`/`ResultColumn` instances.

## M8 — Result metadata & remaining LowLevel surface (2.5 days)

- `columnName`, command-tag-based readonly/statement-kind detection
  (replacing `sqlite3_stmt_readonly`), `PQcmdTuples` exposure
  (replacing per-statement `changes`).
- Table/column origin name via `PQftable`/`PQftablecol` + an OID→name
  catalog lookup (`SELECT relname FROM pg_class WHERE oid = $1`) —
  note in the doc comment that this is an extra round trip not needed
  in the SQLite version, so callers who don't need it should avoid it
  on a hot path.
- **Tests**: mirror `leansqlite`'s coverage of `columnTableName`/
  `columnOriginName`/`columnDatabaseName` where a Postgres analog
  exists.
- **Exit criteria**: parity with the `LowLevel` introspection surface
  in the design doc's module-mapping table, excluding items the
  design doc explicitly drops (`lastInsertRowId`, `totalChanges`,
  `busyTimeout`, `Threading`).

## M9 — Test suite consolidation & CI hardening (3.5 days)

- Port the test framework's success/failure-recording pattern from
  `tests/SQLiteTest/Framework.lean`.
- Adopt the **rollback-per-test** pattern the design doc recommends
  (wrap each test in a transaction, roll back at the end regardless
  of outcome) rather than bespoke setup/teardown SQL per test module
  — this is a nicer fit for Postgres than for SQLite and avoids state
  leaking between test runs sharing one database.
  This is a process change from `leansqlite`'s own test suite, which
  doesn't use this pattern — worth doing here since it removes a
  whole class of test-order-dependency bugs before they occur, not
  after.
- Consolidate the inline tests written alongside M1–M8 into a
  cohesive suite structured like `tests/TestMain.lean` (one
  `testX : TestM Unit` function per feature area, driven from a
  single entry point).
- Finalize the CI Postgres service container configuration (fixed
  version pin, health check, credentials matching what the test
  harness expects).
- **Exit criteria**: `lake test` passes in CI against a live Postgres
  service, run from a clean container each time (no reliance on
  pre-existing schema/data).

## M10 — Documentation & release polish (2.5 days)

- `README.md` mirroring `leansqlite`'s structure: overview, key
  modules, Postgres integration section (replacing "SQLite
  Integration" — cover the libpq build prerequisite explicitly,
  matching the design doc's Build section), development instructions.
- Verso doc comments (`set_option doc.verso true`,
  `set_option linter.missingDocs true`) on all public API, written
  incrementally per milestone rather than retrofitted here if
  possible — call this out to whoever's implementing each milestone
  so it isn't all deferred to M10 in practice even though it's listed
  here as a final consistency pass.
- `LICENSE` (Apache-2.0), initial `CHANGELOG` entry, version tag
  `v0.1.0`.
- **Exit criteria**: README complete, `linter.missingDocs` passes
  with no suppressions, ready to tag.

## Risk register

| Risk | Impact | Mitigation |
|---|---|---|
| libpq header/lib discovery differs across Linux distros, macOS (Homebrew path varies by chip architecture), and Windows | Build breakage for some contributors/CI runners | `pkg-config` as primary path (M0) plus an explicit env-var override; validate the build on at least two platforms before M0 is called done, not just CI's single Linux runner. |
| Postgres array/`numeric` text-format grammar has more edge cases than they first appear (quoting, `NULL` token, precision/scale in `numeric`) | Silent data corruption on round-trip for uncommon inputs | Write edge-case tests *before* declaring M7 done, sourced from the Postgres documentation's array-literal and numeric-type grammar rather than only from values that happen to come up in ad hoc testing. |
| `timestamptz` text representation depends on the session's `TimeZone` setting | Flaky, environment-dependent test failures; possible silent correctness bugs for users who don't pin their session timezone | Pin `TimeZone` explicitly in test setup (M7); document the behavior for library users in the `Timestamptz` doc comment. |
| `Std.Iterators` API churn — `leansqlite`'s own `QueryResult.lean:150-154` already carries a version-compatibility shim for a prior breaking change in that API | A toolchain bump could break `QueryIterator` again mid-project | Track the same Lean toolchain version `leansqlite` currently pins (`lean-toolchain`); don't chase nightly toolchains during initial implementation, and re-check this API specifically before bumping. |
| CI Postgres service-container state leaking between test runs | Flaky/nondeterministic CI | Rollback-per-test pattern (M9) rather than relying on manual cleanup; fresh container per CI run. |
| `IO.Ref`-based mutable state in `Stmt` (new relative to `leansqlite`, which relies on C-side mutation) is a new class of bug surface — e.g. re-entrant use of a `Stmt`, or reading `result`/`cursor` before `step` has been called | Incorrect results or crashes from misuse patterns that couldn't happen in the SQLite version | Get the M2 design reviewed/tested specifically for this before building M3+ on top of it — it's the one piece of the architecture without a direct `leansqlite` precedent to copy. |

## Definition of done for v1

- All milestones M0–M10 complete per their exit criteria above.
- `lake build` and `lake test` both green in CI against a live
  Postgres service, with no manual setup steps beyond what's
  documented in the README.
- Full type catalog from the design doc implemented and tested.
- Public API has Verso doc comments; `linter.missingDocs` passes.
- README documents the libpq build prerequisite and basic usage,
  mirroring `leansqlite`'s README structure.
- Everything in the design doc's Non-goals section (pooling, async
  I/O, streaming/cursors, binary format, LISTEN/NOTIFY, COPY,
  multi-dimensional arrays, ranges, composite types) is explicitly
  out of scope for this plan and not blocking release — captured as
  a v2 backlog, not partially started.

## Suggested tracking

Given `leansqlite`'s git history shows small, focused commits/PRs
(toolchain bumps, individual CI changes), I'd track this as one GitHub
milestone per M0–M10 above, one or more PRs per milestone, each
landing with its own tests rather than one large PR at the end —
consistent with how the existing repo appears to work.
