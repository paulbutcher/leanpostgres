# Changelog

All notable changes to this project are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.1.0] - 2026-07-30

Initial release.

### Added

- Connection layer (`Postgres.open`) and `Postgres.Error`, carrying
  SQLSTATE codes rather than message-text matching.
- Client-side statement buffering and execution (`Postgres.prepare`,
  `Stmt.bindText`/`bindNull`/`step`/`exec`/`columnText`/`columnIsNull`)
  via `PQexecParams`, with no server-side prepared statements in v1.
- `QueryParam`/`ResultColumn`/`Row`/`RowReader`/`QueryIterator` for
  binding query parameters and reading result rows, including a full
  Postgres type catalog: `Bool`, `Int16`/`Int32`/`Int64`,
  `Float32`/`Float`, `String`, `ByteArray`, `Postgres.Numeric`,
  `Postgres.Uuid`, `Std.Time.PlainDate`/`PlainDateTime`/`DateTime`,
  `Postgres.Time` (`time`/`time with time zone`), one-dimensional
  arrays (`Array α`/`Array (Option α)`), and `Unit` (`NULL`).
- `sql!`/`exec!`/`query!` interpolation macros for embedding values
  directly in query text.
- Transactions: `IsolationLevel`, `TransactionOptions`,
  `beginTransaction`/`commit`/`rollback`/`transaction`.
- `deriving` handlers for `Row`, `ResultColumn`, `QueryParam`,
  `ToBinary`, and `FromBinary`.
- Result and command metadata: `columnName`, `columnTableName`,
  `columnOriginName`, `columnDatabaseName`, `commandTag`,
  `commandTuples`, `isReadOnly`.
- A `TestM`-based test suite (ported from `leansqlite`'s test
  framework) covering the above against a live Postgres instance, with
  rollback-per-test isolation.
- CI via GitHub Actions, running against a `postgres` service
  container.
