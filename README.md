# leanpostgres

## Overview

This library provides Lean bindings for Postgres, built directly
against `libpq`. It's designed as a sibling to
[leansqlite](https://github.com/leanprover/leansqlite) — the same
layering and typeclass-driven design, but a natural Postgres API.
The library includes a few conveniences on top of the raw C bindings 
to make working with Postgres more straightforward:

 * Interpolated strings that expand to parameterized queries
 * Type classes for serializing binary blobs, converting data to query
   parameters, and reading columns from results, with `deriving`
   handlers for all of them
 * Iterators over query result rows
 * Transactions with configurable isolation level, read-only, and
   deferrable options
 * A full type catalog covering Postgres's `numeric`, `uuid`,
   `date`/`time`/`timestamp[tz]`, `json`/`jsonb`, and one-dimensional
   array types, alongside the usual boolean/integer/float/text/bytea
   set

## Key Modules

The library is organized into three layers:

The `Postgres.FFI` module contains the raw foreign function interface
bindings to `libpq`, at a very low level of abstraction.

The `Postgres.LowLevel` module wraps the FFI layer with Lean structures
and types that make the API more ergonomic while still staying close
to the underlying C interface. It provides structures for database
connections and statements, transaction options, and result/command
introspection (column names, command tags, affected-row counts). This
module is suitable for users who want a straightforward mapping to
Postgres concepts without additional abstractions.

The main `Postgres` module builds on the low-level API to provide
higher-level conveniences. It includes type classes for binding and
reading values (with the full type catalog above), a row reader monad
for extracting query results, iterator support for working with result
sets, and SQL interpolation syntax for embedding values in queries.

## Postgres Integration

`libpq` is a build-time and runtime prerequisite. You'll need it 
installed before building:

 * Debian/Ubuntu: `apt-get install libpq-dev`
 * Fedora/RHEL: `dnf install postgresql-devel`
 * macOS (Homebrew): `brew install libpq`

The build locates `libpq`'s headers and library via `pkg-config` or
`brew --prefix` by default. If your platform doesn't have
`pkg-config`, or `brew` or `libpq` lives somewhere it can't find, set
`LEANPOSTGRES_PQ_INCLUDE`/ `LEANPOSTGRES_PQ_LIB` to the header/library
directories explicitly.

All values cross the wire as text in v1 (no binary protocol support) —
`PQexecParams` is always called with null parameter/result format
arrays, matching what mainstream Postgres client libraries do by
default. Because parameters are passed through `PQexecParams`'s
separate parameter array rather than interpolated into the SQL string,
there's no client-side SQL escaping/quoting to get right; it's handled
entirely server-side.

## Development

To build the library, use the standard Lake build command from the
repository root. This will compile the `libpq` FFI bindings and the
Lean bindings, producing the library and any default targets.

```bash
lake build
```

Point the standard `PG*` environment variables (`PGHOST`, `PGPORT`,
`PGUSER`, `PGPASSWORD`, `PGDATABASE`) at a reachable server (the
`.devcontainer` setup and CI workflow both include this), then:

```bash
lake test
```

Every test runs inside a transaction that's rolled back afterward
regardless of outcome, so the suite doesn't depend on — or leave
behind — any pre-existing schema or data; it's safe to run repeatedly
against the same database. (A handful of tests that exercise
transaction control itself are the one exception, and clean up their
own tables explicitly instead.)

For verbose output that shows all passing tests in addition to
failures, pass the verbose flag:

```bash
lake test -- --verbose
```

## License

This library is released under the Apache 2.0 license. See the LICENSE
file for the complete license text.
