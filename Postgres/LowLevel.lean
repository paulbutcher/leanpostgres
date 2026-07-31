/-
Copyright (c) 2026 Paul Butcher. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module
import all Postgres.FFI
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

/--
A prepared statement: SQL text plus a client-side buffer of parameters to bind before executing.

There is no server-side prepared-statement object or name — {lit}`prepare` never round-trips
to the server. Parameters and the executed result are held in mutable references because,
unlike {lit}`sqlite3_stmt`, {name}`Stmt` has no backing C object of its own to mutate directly.
-/
structure Stmt where
  /-- The connection this statement executes against. -/
  conn : Conn
  /-- The original SQL text, with {lit}`$1..$N` placeholders. -/
  sql : String
  /-- The number of parameters this statement expects, inferred from the highest {lit}`$N` placeholder found in {lit}`sql`. -/
  paramCount : Nat
  private paramsRef : IO.Ref (Array (Option String))
  private execRef : IO.Ref (Option (FFI.Result × Int32))

/--
Scans {name}`sql` for the highest {lit}`$N` placeholder and returns {lit}`N` (0 if there are
none).

This is a plain text scan, not a SQL parser — a dollar sign inside a string literal or comment
that happens to be followed by digits (e.g. {lit}`'$5 off'`) is indistinguishable from a real
placeholder and would inflate the count. This only risks *over*-counting, though: a phantom
extra parameter slot just stays unbound ({lit}`NULL`), it doesn't shift or misbind a real one.
-/
private def scanParamCount (sql : String) : Nat :=
  go sql.toList none 0
where
  go : List Char → Option Nat → Nat → Nat
  | [], curr, maxSoFar => max maxSoFar (curr.getD 0)
  | '$' :: rest, curr, maxSoFar => go rest (some 0) (max maxSoFar (curr.getD 0))
  | c :: rest, curr, maxSoFar =>
    match curr with
    | some n =>
      if c.isDigit then
        go rest (some (n * 10 + (c.toNat - '0'.toNat))) maxSoFar
      else
        go rest none (max maxSoFar n)
    | none => go rest none maxSoFar

/--
Prepares a statement against {name}`db`.

This is a client-side-only operation — see {name}`Stmt`. {name (full := Stmt.paramCount)}`paramCount`
is found by scanning {name}`sql` for {lit}`$1..$N` placeholders; every parameter starts out
{lit}`NULL` until bound with {lit}`bindText`/{lit}`bindNull`.
-/
def prepare (db : Conn) (sql : String) : IO Stmt := do
  let paramCount := scanParamCount sql
  let paramsRef ← IO.mkRef (Array.replicate paramCount none)
  let execRef ← IO.mkRef none
  return { conn := db, sql, paramCount, paramsRef, execRef }

namespace Stmt

private def checkIndex (stmt : Stmt) (index : Int32) : IO Unit :=
  unless 1 ≤ index && index.toNatClampNeg ≤ stmt.paramCount do
    throw <| IO.userError s!"parameter index {index} out of range (statement has {stmt.paramCount} parameters)"

/-- Binds a string to the host parameter at (1-based) {name}`index`. -/
def bindText (stmt : Stmt) (index : Int32) (value : String) : IO Unit := do
  stmt.checkIndex index
  stmt.paramsRef.modify (·.set! (index.toNatClampNeg - 1) (some value))

/-- Binds {lit}`NULL` to the host parameter at (1-based) {name}`index`. -/
def bindNull (stmt : Stmt) (index : Int32) : IO Unit := do
  stmt.checkIndex index
  stmt.paramsRef.modify (·.set! (index.toNatClampNeg - 1) none)

/--
Executes the statement, advancing to the next row.

The first call flushes the buffered parameters through {lit}`PQexecParams` and stores the
resulting buffered result set; later calls just advance a row cursor already held over that
result. Returns {lean}`true` if a row is available, {lean}`false` if there are no more — or none
at all, e.g. for {lit}`UPDATE`/{lit}`DELETE`/other non-{lit}`SELECT` commands, which never have
rows to begin with.
-/
def step (stmt : Stmt) : IO Bool := do
  match ← stmt.execRef.get with
  | none =>
    let params ← stmt.paramsRef.get
    let result ← FFI.execParams stmt.conn.connection stmt.sql params
    stmt.execRef.set (some (result, 0))
    return FFI.ntuples result > 0
  | some (result, cursor) =>
    let cursor' := cursor + 1
    stmt.execRef.set (some (result, cursor'))
    return cursor' < FFI.ntuples result

/-- Executes a statement, disregarding the availability of results. -/
def exec (stmt : Stmt) : IO Unit := discard stmt.step

private def currentRow (stmt : Stmt) : IO (FFI.Result × Int32) := do
  match ← stmt.execRef.get with
  | some rc => return rc
  | none => throw <| IO.userError "no current row: call `step` (and check that it returned `true`) first"

/-- Extracts the text of the (0-indexed) {name}`column` from the current row. -/
def columnText (stmt : Stmt) (column : Int32) : IO String := do
  let (result, cursor) ← stmt.currentRow
  FFI.getvalue result cursor column

/-- Whether the (0-indexed) {name}`column` is {lit}`NULL` in the current row. -/
def columnIsNull (stmt : Stmt) (column : Int32) : IO Bool := do
  let (result, cursor) ← stmt.currentRow
  return FFI.getisnull result cursor column

private def currentResult (stmt : Stmt) : IO FFI.Result := Prod.fst <$> stmt.currentRow

/--
Returns the name of the (0-indexed) result column — its output name, i.e. its {lit}`AS`-alias if
one was given.

Like every function below, this needs the statement to have already been executed at least once
(see {name}`step`) — unlike SQLite, which compiles/plans a statement upfront at
{name (full := Postgres.prepare)}`prepare` time, Postgres has no result metadata at all until a
command has actually run.
-/
def columnName (stmt : Stmt) (column : Int32) : IO String := do
  FFI.fname (← stmt.currentResult) column

/--
Returns the Postgres command tag of the executed statement (e.g. {lit}`SELECT`, {lit}`INSERT 0 3`,
{lit}`DELETE 1`).
-/
def commandTag (stmt : Stmt) : IO String := do
  FFI.cmdStatus (← stmt.currentResult)

/--
Returns the number of rows affected/retrieved by an {lit}`INSERT`/{lit}`UPDATE`/{lit}`DELETE`/
{lit}`SELECT` (or {lit}`MOVE`/{lit}`FETCH`/{lit}`COPY`), or {lean}`none` if the command doesn't
produce one (e.g. a DDL statement) — replaces SQLite's per-statement {lit}`changes`.
-/
def commandTuples (stmt : Stmt) : IO (Option Int64) := do
  let s ← FFI.cmdTuples (← stmt.currentResult)
  if s.isEmpty then return none
  match s.toInt? with
  | some n => return some (Int64.ofInt n)
  | none => throw <| IO.userError s!"unexpected non-numeric PQcmdTuples value: {s}"

private def readOnlyCommandHeads : List String :=
  ["SELECT", "SHOW", "EXPLAIN", "BEGIN", "COMMIT", "ROLLBACK"]

/--
Whether the executed statement is read-only, derived from its {name}`commandTag` — replaces
SQLite's {lit}`sqlite3_stmt_readonly`, which determines this statically before execution. Postgres
has no equivalent static check (statements aren't parsed/planned client-side here), so this is
only available after {name}`step` has been called at least once, and only classifies the common
cases ({lit}`SELECT`/{lit}`SHOW`/{lit}`EXPLAIN`/{lit}`BEGIN`/{lit}`COMMIT`/{lit}`ROLLBACK`) rather
than being exhaustive over every read-only Postgres command.
-/
def isReadOnly (stmt : Stmt) : IO Bool := do
  let tag ← stmt.commandTag
  return readOnlyCommandHeads.contains ((tag.splitOn " ").headD "")

/-- The table OID a result column directly references, or {lean}`none` for a computed expression. -/
private def columnTableOid (stmt : Stmt) (column : Int32) : IO (Option UInt32) := do
  let oid := FFI.ftable (← stmt.currentResult) column
  return if oid == 0 then none else some oid

/--
Returns the name of the table a result column directly references, via an extra {lit}`pg_class`
lookup on the column's table OID ({lit}`PQftable`). Empty if the column isn't a direct
table-column reference (e.g. a computed expression), or the table has since been dropped.

This is an extra round trip to the server that {name}`columnName` doesn't need — avoid it on a hot
path if you don't need it.
-/
def columnTableName (stmt : Stmt) (column : Int32) : IO String := do
  match ← stmt.columnTableOid column with
  | none => return ""
  | some oid =>
    let lookup ← prepare stmt.conn "SELECT relname FROM pg_class WHERE oid = $1::oid"
    lookup.bindText 1 (toString oid)
    if ← lookup.step then lookup.columnText 0 else return ""

/--
Returns the original column name from the table definition, via {lit}`PQftable`/{lit}`PQftablecol`
plus an extra {lit}`pg_attribute` lookup. For an aliased column (e.g.
{lit}`SELECT user_name AS name`), this returns the underlying column's real name
({lit}`user_name`), while {name}`columnName` returns the alias ({lit}`name`). Empty if the column
isn't a direct table-column reference.

Like {name}`columnTableName`, this is an extra round trip to the server — avoid it on a hot path
if you don't need it.
-/
def columnOriginName (stmt : Stmt) (column : Int32) : IO String := do
  match ← stmt.columnTableOid column with
  | none => return ""
  | some oid =>
    let attnum := FFI.ftablecol (← stmt.currentResult) column
    if attnum == 0 then return "" else
    let lookup ← prepare stmt.conn
      "SELECT attname FROM pg_attribute WHERE attrelid = $1::oid AND attnum = $2::int2"
    lookup.bindText 1 (toString oid)
    lookup.bindText 2 (toString attnum)
    if ← lookup.step then lookup.columnText 0 else return ""

/--
Returns the connection's current database name, ignoring {name}`column`. Unlike SQLite (where
{lit}`ATTACH DATABASE` lets one connection span several databases, hence a per-column database
name), a single Postgres connection always addresses exactly one database — this is simply
{name}`Conn`-level metadata, not something that can vary by result column.
-/
def columnDatabaseName (stmt : Stmt) (column : Int32) : IO String := do
  let _ := column
  FFI.db stmt.conn.connection

end Stmt

/-- Transaction isolation levels. -/
inductive IsolationLevel where
  /-- Allows reads of other transactions' concurrently committed changes; the Postgres default. -/
  | readCommitted
  /-- All reads within the transaction see a consistent snapshot taken at its start. -/
  | repeatableRead
  /-- As {lit}`repeatableRead`, with additional guarantees against write-skew anomalies. -/
  | serializable
deriving Repr, BEq, Hashable, Inhabited, Ord

/-- Options for {lit}`beginTransaction`/{lit}`transaction`. -/
structure TransactionOptions where
  /-- The transaction's isolation level, or {lean}`none` to use the session default. -/
  isolation : Option IsolationLevel := none
  /-- Whether the transaction is read-only. -/
  readOnly : Bool := false
  /-- Whether the transaction may be deferred (only meaningful for a {lit}`serializable`, read-only transaction). -/
  deferrable : Bool := false
deriving Repr, BEq, Hashable, Inhabited

private def isolationLevelSql : IsolationLevel → String
  | .readCommitted => "ISOLATION LEVEL READ COMMITTED"
  | .repeatableRead => "ISOLATION LEVEL REPEATABLE READ"
  | .serializable => "ISOLATION LEVEL SERIALIZABLE"

private def beginTransactionSql (opts : TransactionOptions) : String :=
  let clauses :=
    (match opts.isolation with
      | some level => [isolationLevelSql level]
      | none => []) ++
    (if opts.readOnly then ["READ ONLY"] else []) ++
    (if opts.deferrable then ["DEFERRABLE"] else [])
  match clauses with
  | [] => "BEGIN"
  | _ => "BEGIN " ++ String.intercalate ", " clauses

/-- Begins a transaction with the given options. -/
def beginTransaction (db : Conn) (opts : TransactionOptions := {}) : IO Unit := do
  (← prepare db (beginTransactionSql opts)).exec

/-- Commits the current transaction. -/
def commit (db : Conn) : IO Unit := do
  (← prepare db "COMMIT").exec

/-- Rolls the current transaction back. -/
def rollback (db : Conn) : IO Unit := do
  (← prepare db "ROLLBACK").exec

/--
Executes {name}`action` within a transaction, automatically committing or rolling back.

If {name}`action` succeeds, the transaction is committed. If it throws an exception, the
transaction is rolled back before the exception is re-thrown.
-/
def transaction (db : Conn) (action : IO α) (opts : TransactionOptions := {}) : IO α := do
  beginTransaction db opts
  try
    let result ← action
    commit db
    return result
  catch e =>
    try rollback db
    catch e' => throw <| IO.userError s!"Rollback failed: {e'}\nOriginal error: {e}"
    throw e

end
end Postgres
