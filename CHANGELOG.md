# Changelog

## [0.5.0] - 2026-08-17

- Pools replace connections the database has closed
- Connections are opened as callers need them rather than all at once
- `Pool.withBorrowed` reports whether a session still needs setting up
- `PoolOptions`, `Pool.statistics`, and `Conn.isLive`
- `transaction` no longer discards an error's SQLSTATE when its rollback fails

## [0.4.0] - 2026-08-15

- Move tests into separate package
- Column counts

## [0.3.0] - 2026-08-10

Connection pool

## [0.2.0] - 2026-08-04

Multi-statement SQL scripts

## [0.1.0] - 2026-07-30

Initial release.
