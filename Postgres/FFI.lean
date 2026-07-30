module
namespace Postgres.FFI

@[extern "leanpostgres_initialize"]
private opaque initModule : IO Unit
builtin_initialize initModule

opaque T : NonemptyType.{0}

instance : Nonempty T.type := T.property

public section

/-- Opaque wrapper around a `PGconn*`. -/
def Conn : Type := T.type
deriving Nonempty

public instance : Repr Conn where
  reprPrec _ _ := "#<PGconn *>"

/-- Opaque wrapper around a `PGresult*`. -/
def Result : Type := T.type
deriving Nonempty

public instance : Repr Result where
  reprPrec _ _ := "#<PGresult *>"

@[extern "leanpostgres_open"]
opaque «open» : String → IO Conn

/--
Executes `sql` with the given parameters via `PQexecParams` (text format throughout — every
element of `params` is either `none` for SQL `NULL` or `some` already-encoded text). Returns the
buffered result set, or throws a `Postgres.Error`-shaped error if `PQresultStatus` isn't
`PGRES_TUPLES_OK`/`PGRES_COMMAND_OK`.
-/
@[extern "leanpostgres_exec_params"]
private opaque execParams : @&Conn → String → Array (Option String) → IO Result

/-- The number of rows in a buffered result set. -/
@[extern "leanpostgres_ntuples"]
private opaque ntuples : @&Result → Int32

/-- The number of columns in a buffered result set. -/
@[extern "leanpostgres_nfields"]
private opaque nfields : @&Result → Int32

/-- The text value of `(row, column)` in a buffered result set. Meaningless if the cell is NULL. -/
@[extern "leanpostgres_getvalue"]
private opaque getvalue : @&Result → Int32 → Int32 → IO String

/-- Whether `(row, column)` is SQL `NULL` in a buffered result set. -/
@[extern "leanpostgres_getisnull"]
private opaque getisnull : @&Result → Int32 → Int32 → Bool

/-- The name of a result column (0-indexed), i.e. its output name/alias. -/
@[extern "leanpostgres_fname"]
private opaque fname : @&Result → Int32 → IO String

/-- The command tag of the executed statement (e.g. `"SELECT"`, `"INSERT 0 3"`). -/
@[extern "leanpostgres_cmd_status"]
private opaque cmdStatus : @&Result → IO String

/-- The number of rows affected, as decimal text; empty if the command doesn't produce one. -/
@[extern "leanpostgres_cmd_tuples"]
private opaque cmdTuples : @&Result → IO String

/-- The OID of the table a result column directly references, or `0` if it's a computed expression. -/
@[extern "leanpostgres_ftable"]
private opaque ftable : @&Result → Int32 → UInt32

/-- The attribute number within the table `ftable` identifies, or `0` if none. -/
@[extern "leanpostgres_ftablecol"]
private opaque ftablecol : @&Result → Int32 → Int32

/-- The connection's current database name. -/
@[extern "leanpostgres_db"]
private opaque db : @&Conn → IO String
