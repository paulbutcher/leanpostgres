# Design: a Postgres library inspired by leansqlite

## Status

Draft. Scope decisions locked in for v1: full type catalog, buffered
(non-streaming) result sets, libpq as a build-time and runtime
prerequisite, package/module naming follows `leansqlite`'s
conventions, `json`/`jsonb` represented as `String`, and no
server-side prepared statements (`PQprepare`/`PQexecPrepared`) —
everything routes through `PQexecParams`.

## Goals

- Provide a Lean library for Postgres that feels like a sibling to
  `leansqlite`, not a translation of it: same layering, same
  typeclass-driven binding/reading design, same `deriving` handlers,
  same interpolation macros — but a natural Postgres API, not an
  SQLite-compatibility shim.
- Cover the common Postgres types properly (not just what SQLite
  happens to have), including `numeric`, `timestamp[tz]`, `uuid`,
  `json`/`jsonb`, and one-dimensional arrays.
- Keep v1 simple where SQLite forced complexity that Postgres doesn't
  need (no busy-timeout retry loop, no rowid, no named-parameter
  rewriting), and skip complexity Postgres would need for streaming
  (single-row mode, cursors) since v1 buffers full result sets.
- Use existing Lean libraries tactically where they clearly fit —
  e.g. the toolchain-bundled `Std.Time` for date/time types — rather
  than hand-rolling equivalents or avoiding dependencies as a blanket
  rule. A dependency still needs to earn its place (pull real weight,
  not just avoid a few lines of code), but "dependency-free apart from
  `Std`" is not itself a project goal.

## Non-goals (v1)

- Connection pooling.
- Async/non-blocking I/O (`PQsendQuery`/`PQgetResult`).
- Server-side cursors / `PQsetSingleRowMode` streaming.
- Binary wire format (text format only — see [Wire format](#wire-format)).
- LISTEN/NOTIFY, COPY, multi-dimensional arrays, ranges, composite
  types, custom enum-as-Lean-enum mapping.
- Anything resembling the SQLite scalar-function-extension mechanism
  (`sqlite3_create_function`) — no client-side equivalent exists for
  Postgres. If hashing functions are needed, document `pgcrypto`
  (`digest()`) as the server-side answer instead of porting
  `bindings/shathree.c`.

These are reasonable v2+ candidates once v1 is in use, not things to
design around now.

## Naming

Package name: `leanpostgres` (directory/repo) — `lean` + database
name, exactly mirroring `leansqlite`'s own `lean` + `sqlite` pattern.
Root module namespace: `Postgres`, mirroring `SQLite`. Module files
and paths follow the same convention throughout (`Postgres.lean` as
the root re-export, `Postgres/FFI.lean`, `Postgres/LowLevel.lean`,
etc., matching `SQLite.lean`/`SQLite/FFI.lean`/`SQLite/LowLevel.lean`).

## Architecture: three layers, same shape as leansqlite

```
Postgres.FFI        -- raw C bindings to libpq, opaque external types
Postgres.LowLevel    -- ergonomic Lean wrapper: Conn, Stmt, Value decoding
Postgres              -- QueryParam/ResultColumn/Row, RowReader, QueryIterator,
                         interpolation macros, deriving handlers
```

This mirrors `SQLite.FFI` / `SQLite.LowLevel` / `SQLite` exactly. The
main structural difference is in what `FFI` needs to expose, because
libpq's object model is simpler than SQLite's in one respect (no
persistent per-row cursor object) and requires more client-side
bookkeeping in another (parameter arrays instead of incremental
binds).

### FFI-level types

| leansqlite | leanpostgres | Notes |
|---|---|---|
| `FFI.Conn` (`sqlite3*`) | `FFI.Conn` (`PGconn*`) | External class, finalizer calls `PQfinish`. |
| `FFI.Stmt` (`sqlite3_stmt*`) | *(none — see below)* | v1 has no server-side prepared statements at all (see [Preparing and executing](#preparing-and-executing)) and so no C object to wrap here. |
| `FFI.Value` (`sqlite3_value*`) | *(none)* | Only existed to support the SQLite scalar-function callback API, which isn't ported (see Non-goals). |
| *(implicit: `PGresult*` per step)* | `FFI.Result` (`PGresult*`) | New: buffered result set. External class, finalizer calls `PQclear`. Doesn't exist in the SQLite version because `sqlite3_stmt` *is* its own cursor; here, a completed query produces a distinct object that outlives the statement and is indexed directly. |

Because there's no `sqlite3_stmt`-equivalent pointer, and v1 has no
server-side prepared statement either, `LowLevel.Stmt` becomes a pure
Lean-side record rather than a wrapper around an opaque FFI handle:

```lean
structure Stmt where
  conn : Postgres.Conn
  sql : String                -- original SQL text, with $1..$N placeholders
  paramCount : Nat
  private params : Array (Option ByteArray)  -- text-encoded param buffers; none = NULL
```

`bindText`/`bindInt32`/`bindBlob`/etc. become pure mutations of the
`params` array (via a `StateRefT`-style handle, matching how the
existing library exposes `IO Unit`-returning binds against a mutable
statement). Nothing crosses the FFI boundary until execution.

### Preparing and executing

No server-side prepared statements in v1, per your feedback.
`Postgres.prepare db sql` is a client-side-only operation: it records
the SQL text and computes `paramCount` by scanning for `$1..$N`, with
no round trip to the server. `Stmt.step`/`Stmt.exec` execute the
recorded SQL and buffered param array via `PQexecParams` directly —
there is no `PQprepare`/`PQexecPrepared` call anywhere in v1, and
correspondingly no server-side statement name, no statement-lifetime
management, and no name-collision concerns. This is one fewer round
trip per statement than a prepare/execute split would need, at the
cost of the server re-planning the query on every execution (the
usual prepared-statement trade-off) — acceptable for v1, and adding
real server-side preparation later is compatible with `Stmt`'s public
shape (it's an internal execution-strategy change, not an API change),
so it can be revisited if profiling ever shows repeated-execution
planning overhead matters.

`Stmt.step` (buffered model): the first call executes the query via
`PQexecParams`, wraps the resulting `PGresult` in `FFI.Result`, and
stores it plus a row cursor (`Nat`, starts at 0) in the `Stmt`.
Subsequent `step` calls just increment the cursor and check it against
`PQntuples`. This preserves the existing `Stmt.step : IO Bool` /
`Row`/`QueryIterator` API shape from `QueryResult.lean` essentially
unchanged — `QueryIterator`/`RowReader`/`Row`/`ResultColumn` port with
no conceptual changes, only their underlying `columnText`-style
accessors now index into the buffered `PGresult` (`PQgetvalue`,
`PQgetisnull`) instead of calling `sqlite3_column_*`.

```lean
-- LowLevel.lean, roughly:
def Stmt.step (stmt : Stmt) : IO Bool := do
  match stmt.result with
  | none =>
    let result ← FFI.execParams stmt.conn stmt.sql stmt.params
    -- store result + cursor := 0 in a mutable field
    return (← FFI.ntuples result) > 0
  | some result =>
    -- advance cursor; return cursor < ntuples
    ...
```

This is the one place where the port is architecturally simpler than
SQLite: no per-row FFI round trip, no `SQLITE_BUSY` retry concerns, no
statement-finalization-while-busy hazard (the comment in
`SQLite/FFI.lean:27-33` about connections leaking if closed with
active statements doesn't apply — a `PGresult` is independent of the
connection once returned).

## Wire format

Text format for all values in v1, matching what mainstream Postgres
client libraries (`pg` for Node, `psycopg` for Python) actually do by
default. This is a deliberate simplicity choice, not a shortcut:

- No need to implement Postgres's binary encodings (network-order
  ints, the `numeric` binary format, binary timestamp encoding) for
  v1.
- `PQexecParams` is given a null `paramFormats`/`resultFormats` array
  (or all-zero), meaning text in, text out. Every `QueryParam`
  instance encodes to its Postgres text literal form; every
  `ResultColumn` instance parses from it.
- Because parameters are passed through `PQexecParams`'s separate
  parameter array (not interpolated into the SQL string), there is
  **no client-side SQL escaping/quoting to get right** — this was a
  real risk in the API-compatibility version of this design (which
  needed to reconstruct `expandedSql`-style substituted text by hand);
  it doesn't arise here because Postgres's parameterization is
  handled entirely server-side.
- Binary format is a natural, additive v2 change per-type (the
  `QueryParam`/`ResultColumn` typeclass boundary already isolates
  encoding from the rest of the library), not a redesign.

## Type catalog

Full catalog for v1, per the stated scope. Each row is a
`QueryParam`/`ResultColumn` instance pair (text encode/decode), plus a
`NullableQueryParam`/`NullableResultColumn` instance via `Option`
exactly as in the existing library (`QueryParam.lean:59-74`,
`QueryResult.lean:66-80` — that generic `Option` lifting pattern needs
no changes at all).

| Postgres type | Lean type | Encoding notes |
|---|---|---|
| `boolean` | `Bool` | `t`/`f` text literals. |
| `smallint` (`int2`) | `Int16` *(new — SQLite version has no `Int16` instance)* | Decimal text. |
| `integer` (`int4`) | `Int32` | Decimal text. |
| `bigint` (`int8`) | `Int64` | Decimal text. |
| `real` (`float4`) | `Float32` *(new)* | Postgres text float syntax, incl. `NaN`/`Infinity`. |
| `double precision` (`float8`) | `Float` | Same. |
| `numeric`/`decimal` | `Postgres.Numeric` *(new wrapper type)* | Arbitrary-precision decimal has no native Lean equivalent. Represent as a wrapper around the exact decimal text Postgres returns (sign, digits, scale), not `Float` (lossy) or bare `String` (loses type safety/arithmetic). Arithmetic on `Numeric` itself is out of scope for v1 — it's a faithful carrier type; conversions to `Float`/`Int` are provided where lossy conversion is acceptable to the caller. |
| `text`/`varchar`/`char` | `String` | Direct. |
| `bytea` | `ByteArray` | Postgres text format for bytea is hex (`\x...`); encode/decode hex client-side. (SQLite's `columnBlob`/`bindBlob` move raw bytes over FFI directly — this is a real difference worth noting, not just a rename.) |
| `uuid` | `Postgres.Uuid` *(new wrapper around a 16-byte `ByteArray` or a fixed hex-string form)* | Text form is the standard `8-4-4-4-12` hyphenated hex. |
| `date` | `Std.Time.PlainDate` | ISO `YYYY-MM-DD`. Used directly, no `Postgres`-namespaced wrapper — `Std.Time` (bundled with the toolchain) already provides a validated calendar-date type; Postgres's `QueryParam`/`ResultColumn` instances attach directly to it. Postgres's own text format has more flexibility (variable fractional-second width, trimmed trailing zeros) than `Std.Time`'s bundled format strings parse, so encode/decode goes through a small custom text codec rather than `Std.Time.Formats`. |
| `time`/`time with time zone` | `Postgres.Time` *(new — composes `Std.Time.PlainTime` + optional `Std.Time.TimeZone.Offset`)* | ISO `HH:MM:SS[.ffffff]`, optional offset for `timetz`. `Std.Time` has no bundled type combining a bare time-of-day with an optional zone, so this is a thin composite of two `Std.Time` types rather than a hand-rolled one. |
| `timestamp` | `Std.Time.PlainDateTime` | ISO `YYYY-MM-DD HH:MM:SS[.ffffff]`, no zone. Used directly (no wrapper) — it's exactly `Std.Time`'s date+time-of-day type. |
| `timestamp with time zone` | `Std.Time.DateTime` | Same, plus offset; Postgres always returns these normalized to the session's `TimeZone` setting — worth a doc note since it's a common source of surprise for people new to Postgres. Used directly (no wrapper) via `Std.Time.TimeZone.ofSeconds`/`DateTime.ofPlainDateTimeWithZone`. |
| `json`/`jsonb` | `String` | Per your feedback: `String` for v1. Documented as "the raw JSON text — parse it yourself." A structured `Json`-typed instance (e.g. via the already-bundled `Lean.Json`) is a natural, additive v2 addition (same `QueryParam`/`ResultColumn` boundary), not a redesign, if/when it's wanted. |
| One-dimensional arrays (`int4[]`, `text[]`, ...) | `Array α` given `QueryParam α`/`ResultColumn α` | Postgres's array text literal format (`{a,b,c}`, with quoting/escaping rules for elements containing `,`/`{`/`}`/whitespace/`NULL`) needs a small dedicated parser/printer. Scope to one-dimensional arrays only for v1 — multi-dimensional array text format is a genuinely different grammar and better deferred. |
| `NULL` | `Unit` (param side only, matching `QueryParam Unit` in the existing library) | Direct. |

This is a materially larger surface than SQLite's five-type
(`INTEGER`/`FLOAT`/`TEXT`/`BLOB`/`NULL`) system, and is where most of
the net-new implementation work is, but it's mechanical, uniform work
(one text-encode/decode pair per type) rather than architecturally
risky work.

### Result decoding and OIDs

Unlike SQLite (where `columnType` reflects storage class, and the
existing library explicitly documents that reading via the "wrong"
accessor triggers an implicit, tolerated conversion —
`LowLevel.lean:196` et al.), Postgres results are strictly typed by
OID (`PQftype`). v1's `ResultColumn` instances **do not consult the
column's actual OID** — they trust the caller's declared Lean type and
parse the text accordingly (i.e. calling `Row`/`ResultColumn Int32`
against a `text` column just tries to parse the string as an int32
and throws on failure). This mirrors how most Postgres client
libraries actually behave in practice, and keeps `ResultColumn`
symmetric with `QueryParam` (encode/decode are inverses, independent
of the schema). A stricter mode that validates the OID via `PQftype`
before decoding is a reasonable, additive v2 safety net, not a v1
requirement.

## Connection model

```lean
public def Postgres.open (conninfo : String) : IO Postgres.Conn
```

`conninfo` is a standard libpq connection string or URI
(`"host=localhost dbname=app user=app"` or
`"postgresql://user:pass@host:5432/dbname?sslmode=require"`) —
libpq parses this itself (`PQconnectdb`), so no bespoke parsing is
needed on the Lean side. This replaces `OpenFlags`/`Mode`/`uri`/
`memory`/`threading` wholesale; none of those concepts apply.

Connection failure surfaces as a normal `IO` exception carrying
`PQerrorMessage`, same pattern as `leansqlite_open`'s error path in
`bindings/leansqlite.c:118-130`.

No `busyTimeoutMs` parameter. Postgres's equivalents are ordinary
connection parameters (`connect_timeout`) or session settings
(`SET statement_timeout = ...`), set the natural Postgres way (as part
of `conninfo`, or via `Postgres.exec db "SET statement_timeout = ..."`)
rather than as a bespoke library option.

## Transactions

```lean
public inductive IsolationLevel where
  | readCommitted | repeatableRead | serializable
deriving Repr, BEq, Hashable, Inhabited, Ord

public structure TransactionOptions where
  isolation : Option IsolationLevel := none   -- omit to use the session default
  readOnly : Bool := false
  deferrable : Bool := false
deriving Repr, BEq, Hashable, Inhabited

public def beginTransaction (db : Conn) (opts : TransactionOptions := {}) : IO Unit
```

This replaces `TransactionMode` (`deferred`/`immediate`/`exclusive`)
with Postgres's actual transaction vocabulary rather than reinterpreting
the old enum under the same name — SQLite's locking-hint model has no
honest mapping onto Postgres's isolation-level model, so this is a
clean break, not a compatibility shim. `commit`/`rollback`/`transaction`
(the `try`/`catch` wrapper) port unchanged in spirit from
`LowLevel.lean:641-664`.

## Error handling

Postgres errors carry a SQLSTATE code (`PQresultErrorField(res,
PG_DIAG_SQLSTATE)`) in addition to a human-readable message
(`PQerrorMessage`/`PQresultErrorMessage`). Recommend surfacing both —
worth a small `Postgres.Error` structure (`{ sqlstate : String,
message : String }`) thrown as a Lean exception, rather than
collapsing to a bare string the way `leansqlite_*` error paths do
(`bindings/leansqlite.c`, throughout). SQLSTATE is how callers
distinguish "unique violation" from "serialization failure" from
"syntax error" programmatically, and losing it would force people to
parse the message text, which is fragile and locale-dependent.

## Interpolation macros

`sql!`/`exec!`/`query!` (`SQLite/Interpolation.lean`) port with a
one-line change: the generated placeholder text becomes `s!"${index}"`
instead of `s!"?{index}"` (`Interpolation.lean:54`). Everything else
in the macro — chunk walking, `NullableQueryParam.bind` codegen, the
`sql!`/`exec!`/`query!` surface syntax — is unchanged.

## Deriving handlers

`QueryResult/Deriving.lean` and `Blob/Deriving.lean` (~650 lines
combined) generate `Row`/`ResultColumn` and `ToBinary`/`FromBinary`
instances for user-defined single-constructor types by delegating to
field-level typeclass instances. None of that logic is SQLite-specific
— it ports unchanged once the underlying `ResultColumn`/`QueryParam`
instances exist for the Postgres type catalog above. `Blob.lean`'s
binary-serialization story (`ToBinary`/`FromBinary`, bound to `bytea`
instead of `BLOB`) is likewise a rename, not a redesign.

## Build

Replace the `sqlite.o`/`leansqlite.o`/`shathree.o` custom `target`s in
`lakefile.lean` with:

- A single `bindings/leanpostgres.c` (parallel to
  `bindings/leansqlite.c`) implementing the `FFI.Conn`/`FFI.Result`
  external classes and the `PQ*`-wrapping `LEANSQLITE_API`-style
  exported functions.
- An `extern_lib` target that compiles `leanpostgres.c` against
  libpq's headers and links `-lpq`, located via `pkg-config --cflags
  --libs libpq` (falling back to a configurable include/lib path
  override for platforms without `pkg-config`, e.g. some Windows
  setups).
- No vendored libpq source. Confirmed acceptable per your last
  message — libpq is a build-time and runtime prerequisite
  (`libpq-dev`/`postgresql-devel`/Homebrew `libpq`), same as virtually
  every other language's Postgres driver. Vendoring libpq would also
  mean vendoring OpenSSL for TLS support, which is a separate,
  substantial, ongoing-maintenance undertaking not justified by this
  project's scope.
- `.devcontainer/Dockerfile` needs `libpq-dev` (or equivalent) added
  alongside whatever it currently installs for the Lean toolchain.

## Testing / CI

Requires a live Postgres instance, unlike the current SQLite test
suite (`lake test` against a scratch file, no external service):

- Local dev: add a `postgres` service to `.devcontainer` (or a
  `docker-compose.yml` alongside it) for interactive development.
- CI: add a `services: postgres:` block to
  `.github/workflows/lean_action_ci.yml` (or a new workflow), matching
  the existing job's Lean toolchain setup otherwise.
- Test harness: `tests/SQLiteTest/Framework.lean`'s
  success/failure-recording pattern ports directly; each test module
  should create its own schema/tables in a `setup`/`teardown` pair
  (or a per-test transaction that's rolled back at the end, which is
  actually a nicer fit for Postgres than for SQLite, since
  `ROLLBACK` after a failed assertion cleanly discards all writes
  without needing bespoke cleanup code — recommend this pattern for
  the ported test suite even though the current SQLite tests don't use
  it).

## Module-by-module mapping summary

| leansqlite | leanpostgres | Change |
|---|---|---|
| `SQLite/FFI.lean` | `Postgres/FFI.lean` | New bindings; `Stmt`/`Value` external types dropped, `Result` external type added. |
| `SQLite/LowLevel.lean` | `Postgres/LowLevel.lean` | `OpenFlags`/`Mode`/`Threading` dropped, replaced by conninfo string. `TransactionMode` → `IsolationLevel`/`TransactionOptions`. `lastInsertRowId`/`changes`/`totalChanges` dropped (use `RETURNING`; `PQcmdTuples` exposed directly per-statement if wanted). `busyTimeout` dropped. |
| `SQLite/QueryParam.lean` | `Postgres/QueryParam.lean` | Same typeclass shape; full type catalog above instead of 5 SQLite storage classes. |
| `SQLite/QueryResult.lean` | `Postgres/QueryResult.lean` | Same shape (`ResultColumn`, `RowReader`, `Row`, `QueryIterator`); indexes into a buffered `PGresult` instead of stepping `sqlite3_stmt`. |
| `SQLite/Interpolation.lean` | `Postgres/Interpolation.lean` | Placeholder syntax `?N` → `$N`; otherwise unchanged. |
| `SQLite/Blob.lean` + `Blob/*.lean` | `Postgres/Blob.lean` + `Blob/*.lean` | Rename `BLOB` → `bytea`; hex text encoding instead of raw FFI bytes. |
| `bindings/leansqlite.c`, `sqlite3.c`, `sqlite3*.h` | `bindings/leanpostgres.c` | No vendored server source; links system libpq. |
| `bindings/shathree.c` | *(dropped)* | Document `pgcrypto` as the Postgres-native alternative. |

## Phased plan

1. **Core** (connection, prepare/execute via `PQexecParams`, buffered
   `Stmt`/`QueryIterator`/`Row`, basic type set: `Bool`, `Int16/32/64`,
   `Float32/64`, `String`, `ByteArray`, transactions/isolation levels,
   error type with SQLSTATE, interpolation macros, `deriving` handlers,
   CI with live Postgres).
2. **Full type catalog**: `Numeric`, `Uuid`, `Date`/`Time`/
   `Timestamp`/`Timestamptz`, `json`/`jsonb` (as `String`), 1-D arrays.
3. **Hardening**: parity test suite ported from `tests/TestMain.lean`
   and `tests/SQLiteTest/*.lean` (~3000 lines currently; expect the
   ported suite to be similar in size once the type-catalog tests are
   added, likely larger given the bigger type surface).

Combined estimate for phases 1–3 given the scope settled here (full
catalog, no streaming, no server-side prepared statements, `String`
JSON, `leanpostgres` naming): **7–10 weeks** for one experienced
Lean/C engineer. All prior open questions are resolved (see Status);
this is a firm estimate against the design as written, not a range
pending further decisions.
