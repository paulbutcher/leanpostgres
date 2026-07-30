module
public import Postgres.FFI
public import Postgres.Error

set_option doc.verso true
set_option linter.missingDocs true

namespace Postgres

public section

/-- A connection to a Postgres database. -/
structure Conn where
  /-- The connection string or URI this connection was opened with. -/
  conninfo : String
  /-- The underlying FFI connection object. -/
  connection : FFI.Conn
deriving Repr

/--
Opens a connection to Postgres.

{name}`conninfo` is a standard libpq connection string (e.g.
{lit}`"host=localhost dbname=app user=app"`) or URI (e.g.
{lit}`"postgresql://user:pass@host:5432/dbname?sslmode=require"`) — libpq parses it directly, so
no bespoke parsing happens here. An empty string tells libpq to use its own defaults, which fall
back to the standard {lit}`PG*` environment variables ({lit}`PGHOST`, {lit}`PGUSER`,
{lit}`PGDATABASE`, etc.).

On failure, throws an {name (full := IO.Error)}`IO.Error` carrying a {name}`Postgres.Error`
(recoverable via {name}`Postgres.Error.ofIOError?`) built from libpq's connection error message.
Connection-level failures have no SQLSTATE, so {name}`Postgres.Error.sqlstate` is empty in this
case — a real SQLSTATE only shows up once a connection exists and a statement is executed on it.
-/
def «open» (conninfo : String) : IO Conn := do
  let connection ← FFI.«open» conninfo
  return { conninfo, connection }

end
end Postgres
