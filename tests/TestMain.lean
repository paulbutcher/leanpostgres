import Postgres
import Postgres.Blob.Deriving

open Postgres
open Postgres.Interpolation
open Postgres.Blob

/-!
Smoke test: importing `Postgres` pulls in `Postgres.FFI`, whose
`initModule` runs automatically at startup. If the native binding
object failed to link, or `leanpostgres_initialize` isn't wired up
correctly, this executable would fail to start rather than print.
-/

def checkFFIInitialized : IO Unit :=
  IO.println "leanpostgres: FFI initialized OK"

/-- Connects against the live Postgres instance addressed by the standard `PG*` env vars. -/
def checkConnectSuccess : IO Unit := do
  let conn ← «open» ""
  IO.println s!"connected: {repr conn}"

/--
Connects to a reachable host on a port nothing listens on, so libpq fails fast with
"connection refused" rather than needing a `connect_timeout` to avoid hanging.
-/
def checkConnectFailure : IO Unit := do
  let badConninfo := "host=host.docker.internal port=1 dbname=leanpostgres user=leanpostgres connect_timeout=5"
  let caught ← try
      let _ ← «open» badConninfo
      pure (none : Option IO.Error)
    catch e => pure (some e)
  match caught with
  | none => throw <| IO.userError "expected connecting to a closed port to fail, but it succeeded"
  | some e =>
    match Error.ofIOError? e with
    | none => throw <| IO.userError s!"expected a Postgres.Error, got: {e}"
    | some pgErr =>
      if pgErr.message.isEmpty then
        throw <| IO.userError "expected a non-empty error message on connect failure"
      IO.println s!"connect failure correctly surfaced as a Postgres.Error: {pgErr}"

/-- Parameterized `INSERT`/`SELECT` round trip, including a bound `NULL` and multi-row iteration. -/
def checkParameterizedRoundTrip (conn : Conn) : IO Unit := do
  let create ← prepare conn "CREATE TABLE IF NOT EXISTS leanpostgres_test_stmt (id integer, name text)"
  create.exec
  let clear ← prepare conn "DELETE FROM leanpostgres_test_stmt"
  clear.exec

  let insert1 ← prepare conn "INSERT INTO leanpostgres_test_stmt (id, name) VALUES ($1, $2)"
  insert1.bindText 1 "1"
  insert1.bindText 2 "Alice"
  insert1.exec

  let insert2 ← prepare conn "INSERT INTO leanpostgres_test_stmt (id, name) VALUES ($1, $2)"
  insert2.bindText 1 "2"
  insert2.bindNull 2
  insert2.exec

  let select ← prepare conn "SELECT id, name FROM leanpostgres_test_stmt ORDER BY id"
  let mut hasRow ← select.step
  let mut rows : Array (String × Option String) := #[]
  while hasRow do
    let id ← select.columnText 0
    let name ← if ← select.columnIsNull 1 then pure none else some <$> select.columnText 1
    rows := rows.push (id, name)
    hasRow ← select.step

  let expected := #[("1", some "Alice"), ("2", none)]
  if rows != expected then
    throw <| IO.userError s!"expected {expected}, got {rows}"
  IO.println s!"parameterized round trip OK: {rows}"

/-- `step` on a query matching no rows returns `false` immediately. -/
def checkEmptyResultSet (conn : Conn) : IO Unit := do
  let select ← prepare conn "SELECT id FROM leanpostgres_test_stmt WHERE id = $1"
  select.bindText 1 "999"
  if ← select.step then
    throw <| IO.userError "expected no rows for id = 999"
  IO.println "empty result set correctly returns false immediately"

/-- `UPDATE`/`DELETE` execute fine via `exec` despite never having rows to step over. -/
def checkNonSelectCommands (conn : Conn) : IO Unit := do
  let update ← prepare conn "UPDATE leanpostgres_test_stmt SET name = $1 WHERE id = $2"
  update.bindText 1 "Alicia"
  update.bindText 2 "1"
  update.exec

  let delete ← prepare conn "DELETE FROM leanpostgres_test_stmt WHERE id = $1"
  delete.bindText 1 "2"
  delete.exec

  IO.println "UPDATE/DELETE executed without error"

/-- A syntactically invalid statement surfaces a `Postgres.Error` with SQLSTATE `42601`. -/
def checkMalformedStatementError (conn : Conn) : IO Unit := do
  let badStmt ← prepare conn "SELEKT * FROM nonexistent_syntax_error"
  let caught ← try
      let _ ← badStmt.step
      pure (none : Option IO.Error)
    catch e => pure (some e)
  match caught with
  | none => throw <| IO.userError "expected a syntax error, but the statement succeeded"
  | some e =>
    match Error.ofIOError? e with
    | none => throw <| IO.userError s!"expected a Postgres.Error, got: {e}"
    | some pgErr =>
      if pgErr.sqlstate != "42601" then
        throw <| IO.userError s!"expected SQLSTATE 42601, got: {pgErr}"
      IO.println s!"malformed statement correctly surfaced SQLSTATE 42601: {pgErr}"

/--
Round-trips every M3 core type through `QueryParam`/`ResultColumn`, including bound and read
`NULL` (`Option α`).
-/
def checkCoreTypeRoundTrip (conn : Conn) : IO Unit := do
  let create ← prepare conn
    "CREATE TABLE IF NOT EXISTS leanpostgres_test_types
       (flag boolean, small smallint, n integer, big bigint,
        r real, d double precision, t text, b bytea)"
  create.exec
  let clear ← prepare conn "DELETE FROM leanpostgres_test_types"
  clear.exec

  let insert ← prepare conn
    "INSERT INTO leanpostgres_test_types (flag, small, n, big, r, d, t, b)
       VALUES ($1, $2, $3, $4, $5, $6, $7, $8)"
  insert.bind 1 true
  insert.bind 2 (42 : Int16)
  insert.bind 3 (-7 : Int32)
  insert.bind 4 (9223372036854775807 : Int64)
  insert.bind 5 (3.5 : Float32)
  insert.bind 6 (2.71828 : Float)
  insert.bind 7 "hello"
  insert.bind 8 (ByteArray.mk #[0xDE, 0xAD, 0xBE, 0xEF])
  insert.exec

  let insertNulls ← prepare conn
    "INSERT INTO leanpostgres_test_types (flag, small, n, big, r, d, t, b)
       VALUES ($1, $2, $3, $4, $5, $6, $7, $8)"
  insertNulls.bind 1 (none : Option Bool)
  insertNulls.bind 2 (none : Option Int16)
  insertNulls.bind 3 (none : Option Int32)
  insertNulls.bind 4 (none : Option Int64)
  insertNulls.bind 5 (none : Option Float32)
  insertNulls.bind 6 (none : Option Float)
  insertNulls.bind 7 (none : Option String)
  insertNulls.bind 8 (none : Option ByteArray)
  insertNulls.exec

  let select ← prepare conn
    "SELECT flag, small, n, big, r, d, t, b FROM leanpostgres_test_types ORDER BY small NULLS LAST"

  let mut hasRow ← select.step
  if !hasRow then throw <| IO.userError "expected a first row"
  let flag ← ResultColumn.get (α := Option Bool) select 0
  let small ← ResultColumn.get (α := Option Int16) select 1
  let n ← ResultColumn.get (α := Option Int32) select 2
  let big ← ResultColumn.get (α := Option Int64) select 3
  let r ← ResultColumn.get (α := Option Float32) select 4
  let d ← ResultColumn.get (α := Option Float) select 5
  let t ← ResultColumn.get (α := Option String) select 6
  let b ← ResultColumn.get (α := Option ByteArray) select 7
  if flag != some true then throw <| IO.userError s!"Bool round trip failed: {flag}"
  if small != some (42 : Int16) then throw <| IO.userError s!"Int16 round trip failed: {repr small}"
  if n != some (-7 : Int32) then throw <| IO.userError s!"Int32 round trip failed: {repr n}"
  if big != some (9223372036854775807 : Int64) then
    throw <| IO.userError s!"Int64 round trip failed: {repr big}"
  if r != some (3.5 : Float32) then throw <| IO.userError s!"Float32 round trip failed: {repr r}"
  if d != some (2.71828 : Float) then throw <| IO.userError s!"Float round trip failed: {repr d}"
  if t != some "hello" then throw <| IO.userError s!"String round trip failed: {t}"
  if b != some (ByteArray.mk #[0xDE, 0xAD, 0xBE, 0xEF]) then
    throw <| IO.userError s!"ByteArray round trip failed: {b.map ByteArray.toList}"

  hasRow ← select.step
  if !hasRow then throw <| IO.userError "expected a second (all-NULL) row"
  let flag2 ← ResultColumn.get (α := Option Bool) select 0
  let small2 ← ResultColumn.get (α := Option Int16) select 1
  let n2 ← ResultColumn.get (α := Option Int32) select 2
  let big2 ← ResultColumn.get (α := Option Int64) select 3
  let r2 ← ResultColumn.get (α := Option Float32) select 4
  let d2 ← ResultColumn.get (α := Option Float) select 5
  let t2 ← ResultColumn.get (α := Option String) select 6
  let b2 ← ResultColumn.get (α := Option ByteArray) select 7
  if flag2.isSome || small2.isSome || n2.isSome || big2.isSome || r2.isSome || d2.isSome ||
     t2.isSome || b2.isSome then
    throw <| IO.userError "expected every column of the second row to be NULL"

  IO.println "core type round trip OK (Bool/Int16/Int32/Int64/Float32/Float/String/ByteArray, incl. NULL)"

/--
Uses the tuple `Row` instance and `Stmt.resultsAs` to read a 3-column table and iterate a
multi-row result to completion.
-/
def checkTupleRowIteration (conn : Conn) : IO Unit := do
  let create ← prepare conn
    "CREATE TABLE IF NOT EXISTS leanpostgres_test_rows (id integer, name text, active boolean)"
  create.exec
  let clear ← prepare conn "DELETE FROM leanpostgres_test_rows"
  clear.exec

  for (id, name, active) in
      [((1 : Int32), "Alice", true), (2, "Bob", false), (3, "Carol", true)] do
    let insert ← prepare conn "INSERT INTO leanpostgres_test_rows (id, name, active) VALUES ($1, $2, $3)"
    insert.bind 1 id
    insert.bind 2 name
    insert.bind 3 active
    insert.exec

  let select ← prepare conn "SELECT id, name, active FROM leanpostgres_test_rows ORDER BY id"
  let mut rows : Array (Int32 × String × Bool) := #[]
  for row in select.resultsAs (Int32 × String × Bool) do
    rows := rows.push row

  let expected := #[((1 : Int32), "Alice", true), (2, "Bob", false), (3, "Carol", true)]
  if rows != expected then
    throw <| IO.userError s!"expected {expected}, got {rows}"
  IO.println s!"tuple Row + multi-row iteration OK: {rows}"

/-- Exercises `exec!`/`query!` with a mix of literal SQL and interpolated parameters. -/
def checkInterpolationMacros (conn : Conn) : IO Unit := do
  conn exec!"CREATE TABLE IF NOT EXISTS leanpostgres_test_interp (id integer, name text, active boolean)"
  conn exec!"DELETE FROM leanpostgres_test_interp"

  let people : List (Int32 × String × Bool) := [(1, "Alice", true), (2, "Bob", false), (3, "Carol", true)]
  for (id, name, active) in people do
    conn exec!"INSERT INTO leanpostgres_test_interp (id, name, active) VALUES ({id}, {name}, {active})"

  let minId : Int32 := 2
  let rows ←
    conn query!"SELECT id, name, active FROM leanpostgres_test_interp WHERE id >= {minId} ORDER BY id"
      as (Int32 × String × Bool)
  let mut collected : Array (Int32 × String × Bool) := #[]
  for row in rows do
    collected := collected.push row

  let expected := #[((2 : Int32), "Bob", false), (3, "Carol", true)]
  if collected != expected then
    throw <| IO.userError s!"expected {expected}, got {collected}"
  IO.println s!"interpolation macros OK: {collected}"

/-- `beginTransaction` generates valid `BEGIN` text for every `TransactionOptions` combination. -/
def checkTransactionOptionsCombinations (conn : Conn) : IO Unit := do
  let combos : List TransactionOptions := [
    {},
    { isolation := some .readCommitted },
    { isolation := some .repeatableRead },
    { isolation := some .serializable },
    { readOnly := true },
    { deferrable := true },
    { isolation := some .serializable, readOnly := true, deferrable := true }
  ]
  for opts in combos do
    beginTransaction conn opts
    rollback conn
  IO.println "BEGIN text generation OK for every TransactionOptions combination"

/-- `transaction` commits on success and rolls back on a thrown exception. -/
def checkTransactionCommitAndRollback (conn : Conn) : IO Unit := do
  let create ← prepare conn "CREATE TABLE IF NOT EXISTS leanpostgres_test_txn (id integer)"
  create.exec
  let clear ← prepare conn "DELETE FROM leanpostgres_test_txn"
  clear.exec

  let _ ← transaction conn (do
    let insert ← prepare conn "INSERT INTO leanpostgres_test_txn (id) VALUES (1)"
    insert.exec)
  let select ← prepare conn "SELECT id FROM leanpostgres_test_txn WHERE id = 1"
  if !(← select.step) then
    throw <| IO.userError "expected the committed row to be visible after transaction succeeded"

  let caught ← try
      let _ ← transaction conn (do
        let insert ← prepare conn "INSERT INTO leanpostgres_test_txn (id) VALUES (2)"
        insert.exec
        throw <| IO.userError "boom" : IO Unit)
      pure (none : Option IO.Error)
    catch e => pure (some e)
  if caught.isNone then
    throw <| IO.userError "expected the action's exception to propagate out of `transaction`"

  let select2 ← prepare conn "SELECT id FROM leanpostgres_test_txn WHERE id = 2"
  if ← select2.step then
    throw <| IO.userError "expected the rolled-back row to be absent after transaction threw"

  IO.println "transaction commit/rollback OK"

/--
A unique-constraint violation surfaces as SQLSTATE `23505`, not just a message string — asserting
on the code (not the message) is the behavior callers are meant to rely on.
-/
def checkUniqueViolationSqlstate (conn : Conn) : IO Unit := do
  let create ← prepare conn
    "CREATE TABLE IF NOT EXISTS leanpostgres_test_unique (id integer PRIMARY KEY)"
  create.exec
  let clear ← prepare conn "DELETE FROM leanpostgres_test_unique"
  clear.exec
  let insert1 ← prepare conn "INSERT INTO leanpostgres_test_unique (id) VALUES (1)"
  insert1.exec

  let insert2 ← prepare conn "INSERT INTO leanpostgres_test_unique (id) VALUES (1)"
  let caught ← try
      insert2.exec
      pure (none : Option IO.Error)
    catch e => pure (some e)
  match caught with
  | none => throw <| IO.userError "expected a unique constraint violation, but the insert succeeded"
  | some e =>
    match Error.ofIOError? e with
    | none => throw <| IO.userError s!"expected a Postgres.Error, got: {e}"
    | some pgErr =>
      if pgErr.sqlstate != "23505" then
        throw <| IO.userError s!"expected SQLSTATE 23505, got: {pgErr}"
      IO.println s!"unique violation correctly surfaced SQLSTATE 23505: {pgErr}"

/-- Approximate equality for floats with a tolerance of 1e-9. -/
def approxEq (a b : Float) (epsilon : Float := 1e-9) : Bool :=
  (a - b).abs < epsilon

/-- Approximate equality for optional floats. -/
def optApproxEq (a b : Option Float) : Bool :=
  match a, b with
  | some x, some y => approxEq x y
  | none, none => true
  | _, _ => false

/-! ## Person: basic `Row` deriving -/

structure Person where
  name : String
  age : Int32
deriving Repr, BEq, Row

/-- `Row` deriving for a basic multi-field structure, incl. reading through `resultsAs`/`query!`. -/
def checkPersonRowDeriving (conn : Conn) : IO Unit := do
  conn exec!"CREATE TABLE IF NOT EXISTS leanpostgres_test_people (name text NOT NULL, age integer NOT NULL)"
  conn exec!"DELETE FROM leanpostgres_test_people"
  for (name, age) in [("Alice", (30 : Int32)), ("Bob", 25), ("Charlie", 35)] do
    conn exec!"INSERT INTO leanpostgres_test_people (name, age) VALUES ({name}, {age})"

  let people ← ((← prepare conn "SELECT name, age FROM leanpostgres_test_people ORDER BY age").resultsAs Person).toArray
  let expected : Array Person :=
    #[{ name := "Bob", age := 25 }, { name := "Alice", age := 30 }, { name := "Charlie", age := 35 }]
  if people != expected then throw <| IO.userError s!"expected {repr expected}, got {repr people}"

  let filtered ← (← conn query!"SELECT name, age FROM leanpostgres_test_people WHERE age > {(27 : Int32)}" as Person).toArray
  let expectedFiltered : Array Person := #[{ name := "Alice", age := 30 }, { name := "Charlie", age := 35 }]
  if filtered != expectedFiltered then
    throw <| IO.userError s!"expected {repr expectedFiltered}, got {repr filtered}"
  IO.println s!"Person Row deriving OK: {repr people}"

/-! ## NullablePerson: `Option` field handling -/

structure NullablePerson where
  name : String
  nickname : Option String
  age : Int32
deriving Repr, BEq, Row

/-- `Row` deriving handles `Option` fields (`NULL`) correctly. -/
def checkNullablePersonRowDeriving (conn : Conn) : IO Unit := do
  conn exec!"CREATE TABLE IF NOT EXISTS leanpostgres_test_nullable_people
               (name text NOT NULL, nickname text, age integer NOT NULL)"
  conn exec!"DELETE FROM leanpostgres_test_nullable_people"
  conn exec!"INSERT INTO leanpostgres_test_nullable_people (name, nickname, age) VALUES ('Alice', 'Ali', 30)"
  conn exec!"INSERT INTO leanpostgres_test_nullable_people (name, nickname, age) VALUES ('Bob', NULL, 25)"

  let select ← prepare conn "SELECT name, nickname, age FROM leanpostgres_test_nullable_people ORDER BY name"
  let people ← (select.resultsAs NullablePerson).toArray
  let expected : Array NullablePerson :=
    #[{ name := "Alice", nickname := some "Ali", age := 30 }, { name := "Bob", nickname := none, age := 25 }]
  if people != expected then throw <| IO.userError s!"expected {repr expected}, got {repr people}"
  IO.println s!"NullablePerson Row deriving OK: {repr people}"

/-! ## Product: a mix of field types -/

structure Product where
  id : Int64
  name : String
  price : Float
  inStock : Bool
deriving Repr, Inhabited, Row

instance : BEq Product where
  beq a b := a.id == b.id && a.name == b.name && approxEq a.price b.price && a.inStock == b.inStock

/-- `Row` deriving across `Int64`/`String`/`Float`/`Bool` fields together. -/
def checkProductRowDeriving (conn : Conn) : IO Unit := do
  conn exec!"CREATE TABLE IF NOT EXISTS leanpostgres_test_products
               (id bigint NOT NULL, name text NOT NULL, price double precision NOT NULL, in_stock boolean NOT NULL)"
  conn exec!"DELETE FROM leanpostgres_test_products"
  conn exec!"INSERT INTO leanpostgres_test_products (id, name, price, in_stock) VALUES (1, 'Widget', 19.99, true)"
  conn exec!"INSERT INTO leanpostgres_test_products (id, name, price, in_stock) VALUES (2, 'Gadget', 29.99, false)"

  let select ← prepare conn "SELECT id, name, price, in_stock FROM leanpostgres_test_products ORDER BY id"
  let products ← (select.resultsAs Product).toArray
  let expected : Array Product :=
    #[{ id := 1, name := "Widget", price := 19.99, inStock := true },
      { id := 2, name := "Gadget", price := 29.99, inStock := false }]
  if products != expected then throw <| IO.userError s!"expected {repr expected}, got {repr products}"
  IO.println s!"Product Row deriving OK: {repr products}"

/-! ## AllOptional: every field is `Option` -/

structure AllOptional where
  a : Option String
  b : Option Int32
  c : Option Float
deriving Repr, Inhabited, Row

instance : BEq AllOptional where
  beq x y := x.a == y.a && x.b == y.b && optApproxEq x.c y.c

/-- `Row` deriving when every field is nullable. -/
def checkAllOptionalRowDeriving (conn : Conn) : IO Unit := do
  conn exec!"CREATE TABLE IF NOT EXISTS leanpostgres_test_all_optional (a text, b integer, c double precision)"
  conn exec!"DELETE FROM leanpostgres_test_all_optional"
  conn exec!"INSERT INTO leanpostgres_test_all_optional (a, b, c) VALUES ('test', 42, 3.14)"
  conn exec!"INSERT INTO leanpostgres_test_all_optional (a, b, c) VALUES (NULL, NULL, NULL)"

  let select ← prepare conn "SELECT a, b, c FROM leanpostgres_test_all_optional ORDER BY a NULLS LAST"
  let rows ← (select.resultsAs AllOptional).toArray
  let expected : Array AllOptional :=
    #[{ a := some "test", b := some 42, c := some 3.14 }, { a := none, b := none, c := none }]
  if rows != expected then throw <| IO.userError s!"expected {repr expected}, got {repr rows}"
  IO.println s!"AllOptional Row deriving OK: {repr rows}"

/-! ## EmptyRow: a zero-field structure -/

structure EmptyRow where
deriving Repr, BEq, Row

/-- `Row` deriving for a zero-field structure reads no columns, one `EmptyRow` per row. -/
def checkEmptyRowDeriving (conn : Conn) : IO Unit := do
  conn exec!"CREATE TABLE IF NOT EXISTS leanpostgres_test_empty_row (dummy integer)"
  conn exec!"DELETE FROM leanpostgres_test_empty_row"
  conn exec!"INSERT INTO leanpostgres_test_empty_row (dummy) VALUES (1)"
  conn exec!"INSERT INTO leanpostgres_test_empty_row (dummy) VALUES (2)"
  conn exec!"INSERT INTO leanpostgres_test_empty_row (dummy) VALUES (3)"

  let select ← prepare conn "SELECT 1 FROM leanpostgres_test_empty_row"
  let empties ← (select.resultsAs EmptyRow).toArray
  if empties.size != 3 then throw <| IO.userError s!"expected 3 empty rows, got {empties.size}"
  IO.println s!"EmptyRow Row deriving OK: {empties.size} rows"

/-! ## UserId/Username: trivial wrappers with `ResultColumn`/`QueryParam` -/

structure UserId where
  id : Int64
deriving Repr, BEq, Inhabited, ResultColumn, QueryParam

structure Username where
  name : String
deriving Repr, BEq, Inhabited, ResultColumn, QueryParam

/-- `ResultColumn`/`QueryParam` deriving for trivial single-field wrapper types. -/
def checkWrapperTypeDeriving (conn : Conn) : IO Unit := do
  conn exec!"CREATE TABLE IF NOT EXISTS leanpostgres_test_wrapper_users (id bigint NOT NULL, username text NOT NULL)"
  conn exec!"DELETE FROM leanpostgres_test_wrapper_users"
  conn exec!"INSERT INTO leanpostgres_test_wrapper_users (id, username) VALUES (1, 'alice')"
  conn exec!"INSERT INTO leanpostgres_test_wrapper_users (id, username) VALUES (2, 'bob')"

  let userIds ← ((← prepare conn "SELECT id FROM leanpostgres_test_wrapper_users ORDER BY id").resultsAs UserId).toArray
  if userIds != #[(⟨1⟩ : UserId), ⟨2⟩] then throw <| IO.userError s!"expected user ids, got {repr userIds}"

  let targetId : UserId := ⟨1⟩
  let namesById ← (← conn query!"SELECT username FROM leanpostgres_test_wrapper_users WHERE id = {targetId}" as Username).toArray
  if namesById != #[(⟨"alice"⟩ : Username)] then throw <| IO.userError s!"expected alice, got {repr namesById}"

  let usernames ←
    ((← prepare conn "SELECT username FROM leanpostgres_test_wrapper_users ORDER BY id").resultsAs Username).toArray
  if usernames != #[(⟨"alice"⟩ : Username), ⟨"bob"⟩] then
    throw <| IO.userError s!"expected usernames, got {repr usernames}"

  -- Trivial wrappers also work as ordinary `ResultColumn`-backed tuple fields.
  let mixed ←
    ((← prepare conn "SELECT id, username FROM leanpostgres_test_wrapper_users ORDER BY id").resultsAs
      (UserId × Username)).toArray
  if mixed != #[((⟨1⟩ : UserId), (⟨"alice"⟩ : Username)), (⟨2⟩, ⟨"bob"⟩)] then
    throw <| IO.userError s!"expected id/username pairs, got {repr mixed}"

  let targetName : Username := ⟨"bob"⟩
  let idsByName ← (← conn query!"SELECT id FROM leanpostgres_test_wrapper_users WHERE username = {targetName}" as UserId).toArray
  if idsByName != #[(⟨2⟩ : UserId)] then throw <| IO.userError s!"expected bob's id, got {repr idsByName}"

  IO.println "UserId/Username ResultColumn/QueryParam deriving OK"

/-! ## Coordinate/Email: non-structure inductive types -/

inductive Coordinate where
  | mk (x : Float) (y : Float)
deriving Repr, BEq, Inhabited, Row

/-- `Row` deriving works for a non-structure (single-constructor) inductive type. -/
def checkCoordinateRowDeriving (conn : Conn) : IO Unit := do
  conn exec!"CREATE TABLE IF NOT EXISTS leanpostgres_test_coordinates (x double precision NOT NULL, y double precision NOT NULL)"
  conn exec!"DELETE FROM leanpostgres_test_coordinates"
  conn exec!"INSERT INTO leanpostgres_test_coordinates (x, y) VALUES (1.0, 2.0)"
  conn exec!"INSERT INTO leanpostgres_test_coordinates (x, y) VALUES (3.5, 4.5)"

  let select ← prepare conn "SELECT x, y FROM leanpostgres_test_coordinates ORDER BY x"
  let coords ← (select.resultsAs Coordinate).toArray
  let expected : Array Coordinate := #[.mk 1.0 2.0, .mk 3.5 4.5]
  if coords != expected then throw <| IO.userError s!"expected {repr expected}, got {repr coords}"
  IO.println s!"Coordinate Row deriving OK: {repr coords}"

inductive Email where
  | mk (addr : String)
deriving Repr, BEq, Inhabited, ResultColumn, QueryParam

/-- `ResultColumn`/`QueryParam` deriving works for a non-structure (single-field) inductive type. -/
def checkEmailDeriving (conn : Conn) : IO Unit := do
  conn exec!"CREATE TABLE IF NOT EXISTS leanpostgres_test_emails (addr text NOT NULL)"
  conn exec!"DELETE FROM leanpostgres_test_emails"
  conn exec!"INSERT INTO leanpostgres_test_emails (addr) VALUES ('alice@example.com')"
  conn exec!"INSERT INTO leanpostgres_test_emails (addr) VALUES ('bob@example.com')"

  let select ← prepare conn "SELECT addr FROM leanpostgres_test_emails ORDER BY addr"
  let emails ← (select.resultsAs Email).toArray
  let expected : Array Email := #[.mk "alice@example.com", .mk "bob@example.com"]
  if emails != expected then throw <| IO.userError s!"expected {repr expected}, got {repr emails}"

  let target : Email := .mk "bob@example.com"
  let byAddr ← (← conn query!"SELECT addr FROM leanpostgres_test_emails WHERE addr = {target}" as Email).toArray
  if byAddr != #[Email.mk "bob@example.com"] then throw <| IO.userError s!"expected bob's email, got {repr byAddr}"
  IO.println s!"Email ResultColumn/QueryParam deriving OK: {repr emails}"

/-! ## NonEmptyString: `QueryParam` ignores proof fields -/

structure NonEmptyString where
  val : String
  nonEmpty : val.length > 0
deriving QueryParam

/-- `QueryParam` deriving ignores proof fields — only `val` is bound. -/
def checkNonEmptyStringQueryParamDeriving (conn : Conn) : IO Unit := do
  conn exec!"CREATE TABLE IF NOT EXISTS leanpostgres_test_nonempty_strings (val text NOT NULL)"
  conn exec!"DELETE FROM leanpostgres_test_nonempty_strings"
  conn exec!"INSERT INTO leanpostgres_test_nonempty_strings (val) VALUES ('hello')"
  conn exec!"INSERT INTO leanpostgres_test_nonempty_strings (val) VALUES ('world')"

  let target : NonEmptyString := ⟨"hello", by decide⟩
  let hits ← (← conn query!"SELECT val FROM leanpostgres_test_nonempty_strings WHERE val = {target}" as String).toArray
  if hits != #["hello"] then throw <| IO.userError s!"expected hello, got {hits}"
  IO.println "NonEmptyString QueryParam deriving (proof field ignored) OK"

/-! ## Negative compile-time tests: rejected shapes -/

/-- error: None of the deriving handlers for class `Row` applied to `MultiCtorForRow` -/
#guard_msgs in
inductive MultiCtorForRow where
  | a (x : Int32) | b (y : String)
deriving Row

/-- error: None of the deriving handlers for class `Row` applied to `ProofFieldForRow` -/
#guard_msgs in
structure ProofFieldForRow where
  val : Int32
  inBounds : val ≥ 0
deriving Row

/-- error: None of the deriving handlers for class `ResultColumn` applied to `MultiCtorForResultColumn` -/
#guard_msgs in
inductive MultiCtorForResultColumn where
  | a (x : Int32) | b (y : String)
deriving ResultColumn

/-- error: None of the deriving handlers for class `ResultColumn` applied to `MultiFieldForResultColumn` -/
#guard_msgs in
structure MultiFieldForResultColumn where
  x : Int32
  y : String
deriving ResultColumn

/-- error: None of the deriving handlers for class `ResultColumn` applied to `ZeroFieldForResultColumn` -/
#guard_msgs in
structure ZeroFieldForResultColumn where
deriving ResultColumn

/--
error: failed to synthesize instance of type class
  ResultColumn (Option RecursiveMissingInstance)

Hint: Type class instance resolution failures can be inspected with the `set_option trace.Meta.synthInstance true` command.
-/
#guard_msgs in
structure RecursiveMissingInstance where
  next : Option RecursiveMissingInstance
deriving ResultColumn

/-- error: None of the deriving handlers for class `QueryParam` applied to `MultiCtorForQueryParam` -/
#guard_msgs in
inductive MultiCtorForQueryParam where
  | inl (n : Nat) | inr (b : Bool)
deriving QueryParam

/-- error: None of the deriving handlers for class `QueryParam` applied to `MultiDataFieldForQueryParam` -/
#guard_msgs in
structure MultiDataFieldForQueryParam where
  x : Int32
  y : String
deriving QueryParam

/-- error: None of the deriving handlers for class `QueryParam` applied to `ZeroFieldForQueryParam` -/
#guard_msgs in
structure ZeroFieldForQueryParam where
deriving QueryParam

/-- error: None of the deriving handlers for class `QueryParam` applied to `OnlyProofFields` -/
#guard_msgs in
structure OnlyProofFields where
  h : 1 + 1 = 2
deriving QueryParam

/-! ## Blob deriving: `ToBinary`/`FromBinary` -/

structure Pair where
  x : Nat
  y : String
deriving BEq, Repr, ToBinary, FromBinary

inductive Color where
  | r | g | b
deriving BEq, Repr, Inhabited, ToBinary, FromBinary

inductive Shape where
  | circle (radius : Nat)
  | rect (w : Nat) (h : Nat)
deriving BEq, Repr, ToBinary, FromBinary

inductive Msg where
  | flagged (b : Bool) (s : String)
  | plain (s : String)
deriving BEq, Repr, ToBinary, FromBinary

inductive Cmd where
  | exec (retries : Option Nat) (cmd : String)
  | noop
deriving BEq, Repr, ToBinary, FromBinary

structure Box (α : Type) where
  val : α
deriving BEq, Repr, ToBinary, FromBinary

inductive Tree where
  | leaf (val : Nat)
  | node (left : Tree) (right : Tree)
deriving BEq, Repr, ToBinary, FromBinary

/-- error: None of the deriving handlers for class `ToBinary` applied to `ProofField` -/
#guard_msgs in
structure ProofField where
  val : Nat
  pos : val > 0
deriving ToBinary

/-- error: None of the deriving handlers for class `FromBinary` applied to `ProofField2` -/
#guard_msgs in
structure ProofField2 where
  val : Nat
  pos : val > 0
deriving FromBinary

inductive NoConstructors
deriving ToBinary, FromBinary

/-- Serializes then deserializes `x`, checking the result matches. -/
def roundTrips [ToBinary α] [FromBinary α] [BEq α] (x : α) : Bool :=
  match fromBinary (toBinary x) with
  | .ok y => x == y
  | .error _ => false

/-- `ToBinary`/`FromBinary` deriving round-trips single-ctor, multi-ctor, parametric, and recursive types. -/
def checkBlobDeriving : IO Unit := do
  let checks : List Bool := [
    roundTrips (Pair.mk 42 "hello"),
    roundTrips Color.r, roundTrips Color.g, roundTrips Color.b,
    roundTrips (Shape.circle 5), roundTrips (Shape.rect 3 4),
    roundTrips (Msg.flagged true "hi"), roundTrips (Msg.plain "hi"),
    roundTrips (Cmd.exec (some 3) "go"), roundTrips (Cmd.exec none "go"), roundTrips Cmd.noop,
    roundTrips (Box.mk (5 : Nat)), roundTrips (Box.mk "hi"),
    roundTrips (Tree.node (.leaf 1) (.node (.leaf 2) (.leaf 3)))
  ]
  if !checks.all id then
    throw <| IO.userError s!"expected every Blob deriving round trip to succeed, got {checks}"

  match fromBinaryOf NoConstructors .empty with
  | .error msg =>
    if msg != "Cannot deserialize uninhabited type `NoConstructors`" then
      throw <| IO.userError s!"unexpected error message for uninhabited type: {msg}"
  | .ok _ => throw <| IO.userError "expected deserializing an uninhabited type to fail"

  IO.println "Blob ToBinary/FromBinary deriving OK"

/-- Round-trips `Postgres.Numeric`, including `NaN`/`Infinity`/`-Infinity` and trailing-zero scale. -/
def checkNumericRoundTrip (conn : Conn) : IO Unit := do
  let create ← prepare conn "CREATE TABLE IF NOT EXISTS leanpostgres_test_numeric (id integer, n numeric)"
  create.exec
  let clear ← prepare conn "DELETE FROM leanpostgres_test_numeric"
  clear.exec

  let values : Array (Int32 × Numeric) := #[
    (1, .ofDigits false 1234500 4),
    (2, .ofDigits true 1 3),
    (3, .ofDigits false 0 0),
    (4, .ofDigits false 100 0),
    (5, .nan),
    (6, .posInf),
    (7, .negInf)
  ]
  for (id, n) in values do
    let insert ← prepare conn "INSERT INTO leanpostgres_test_numeric (id, n) VALUES ($1, $2)"
    insert.bind 1 id
    insert.bind 2 n
    insert.exec

  for (id, expected) in values do
    let select ← prepare conn "SELECT n FROM leanpostgres_test_numeric WHERE id = $1"
    select.bind 1 id
    let hasRow ← select.step
    if !hasRow then throw <| IO.userError s!"expected a row for id {id}"
    let n ← ResultColumn.get (α := Numeric) select 0
    if n != expected then
      throw <| IO.userError s!"Numeric round trip failed for id {id}: expected {repr expected}, got {repr n}"

  IO.println "Numeric round trip OK (incl. NaN/Infinity/-Infinity, trailing zeros)"

/-- Round-trips `Postgres.Uuid` through a real `uuid` column. -/
def checkUuidRoundTrip (conn : Conn) : IO Unit := do
  let create ← prepare conn "CREATE TABLE IF NOT EXISTS leanpostgres_test_uuid (id integer, u uuid)"
  create.exec
  let clear ← prepare conn "DELETE FROM leanpostgres_test_uuid"
  clear.exec

  match Uuid.ofText? "123e4567-e89b-12d3-a456-426614174000" with
  | none => throw <| IO.userError "failed to parse test uuid literal"
  | some uuid =>
    let insert ← prepare conn "INSERT INTO leanpostgres_test_uuid (id, u) VALUES ($1, $2)"
    insert.bind 1 (1 : Int32)
    insert.bind 2 uuid
    insert.exec

    let select ← prepare conn "SELECT u FROM leanpostgres_test_uuid WHERE id = $1"
    select.bind 1 (1 : Int32)
    let hasRow ← select.step
    if !hasRow then throw <| IO.userError "expected a row"
    let result ← ResultColumn.get (α := Uuid) select 0
    if result != uuid then
      throw <| IO.userError s!"Uuid round trip failed: expected {uuid.toText}, got {result.toText}"
    IO.println s!"Uuid round trip OK ({result.toText})"

/-- Round-trips `Std.Time.PlainDate` through a real `date` column, incl. a leap day. -/
def checkDateRoundTrip (conn : Conn) : IO Unit := do
  let create ← prepare conn "CREATE TABLE IF NOT EXISTS leanpostgres_test_date (id integer, d date)"
  create.exec
  let clear ← prepare conn "DELETE FROM leanpostgres_test_date"
  clear.exec

  match dateOfText? "2024-02-29" with
  | none => throw <| IO.userError "failed to parse test date literal"
  | some d =>
    let insert ← prepare conn "INSERT INTO leanpostgres_test_date (id, d) VALUES ($1, $2)"
    insert.bind 1 (1 : Int32)
    insert.bind 2 d
    insert.exec

    let select ← prepare conn "SELECT d FROM leanpostgres_test_date WHERE id = $1"
    select.bind 1 (1 : Int32)
    let hasRow ← select.step
    if !hasRow then throw <| IO.userError "expected a row"
    let result ← ResultColumn.get (α := Std.Time.PlainDate) select 0
    if result != d then
      throw <| IO.userError s!"date round trip failed: expected {dateToText d}, got {dateToText result}"

  if (dateOfText? "2026-02-30").isSome then
    throw <| IO.userError "expected Feb 30 to be rejected as an invalid date"

  IO.println "Date round trip OK (incl. leap day, invalid-date rejection)"

/-- Round-trips `Postgres.Time` through both `time` (no zone) and `time with time zone` columns. -/
def checkTimeRoundTrip (conn : Conn) : IO Unit := do
  let create ← prepare conn
    "CREATE TABLE IF NOT EXISTS leanpostgres_test_time (id integer, t time, tz timetz)"
  create.exec
  let clear ← prepare conn "DELETE FROM leanpostgres_test_time"
  clear.exec

  match Time.ofText? "13:05:07.5", Time.ofText? "13:05:07+05:30" with
  | none, _ | _, none => throw <| IO.userError "failed to parse test time literals"
  | some plain, some withOffset =>
    let insert ← prepare conn "INSERT INTO leanpostgres_test_time (id, t, tz) VALUES ($1, $2, $3)"
    insert.bind 1 (1 : Int32)
    insert.bind 2 plain
    insert.bind 3 withOffset
    insert.exec

    let select ← prepare conn "SELECT t, tz FROM leanpostgres_test_time WHERE id = $1"
    select.bind 1 (1 : Int32)
    let hasRow ← select.step
    if !hasRow then throw <| IO.userError "expected a row"
    let resultPlain ← ResultColumn.get (α := Time) select 0
    let resultTz ← ResultColumn.get (α := Time) select 1
    if resultPlain != plain then
      throw <| IO.userError s!"time round trip failed: expected {plain.toText}, got {resultPlain.toText}"
    -- unlike timestamptz, Postgres preserves timetz's original offset verbatim (no session-zone
    -- normalization), so this should be an exact round trip, not just "has some offset".
    if resultTz != withOffset then
      throw <| IO.userError s!"timetz round trip failed: expected {withOffset.toText}, got {resultTz.toText}"
    IO.println s!"Time/timetz round trip OK ({resultPlain.toText}, {resultTz.toText})"

/-- Round-trips `Std.Time.PlainDateTime` through a real `timestamp` column. -/
def checkTimestampRoundTrip (conn : Conn) : IO Unit := do
  let create ← prepare conn "CREATE TABLE IF NOT EXISTS leanpostgres_test_timestamp (id integer, ts timestamp)"
  create.exec
  let clear ← prepare conn "DELETE FROM leanpostgres_test_timestamp"
  clear.exec

  match timestampOfText? "2026-07-30 13:05:07.5" with
  | none => throw <| IO.userError "failed to parse test timestamp literal"
  | some ts =>
    let insert ← prepare conn "INSERT INTO leanpostgres_test_timestamp (id, ts) VALUES ($1, $2)"
    insert.bind 1 (1 : Int32)
    insert.bind 2 ts
    insert.exec

    let select ← prepare conn "SELECT ts FROM leanpostgres_test_timestamp WHERE id = $1"
    select.bind 1 (1 : Int32)
    let hasRow ← select.step
    if !hasRow then throw <| IO.userError "expected a row"
    let result ← ResultColumn.get (α := Std.Time.PlainDateTime) select 0
    if result != ts then
      throw <| IO.userError
        s!"timestamp round trip failed: expected {timestampToText ts}, got {timestampToText result}"
    IO.println s!"Timestamp round trip OK ({timestampToText result})"

/--
Round-trips `Std.Time.DateTime` through a real `timestamptz` column, with the session `TimeZone`
explicitly pinned to UTC first — Postgres normalizes `timestamptz` display to the session zone, so
an unpinned test would be a flaky, environment-dependent check.
-/
def checkTimestamptzRoundTrip (conn : Conn) : IO Unit := do
  let setTz ← prepare conn "SET TimeZone = 'UTC'"
  setTz.exec

  let create ← prepare conn
    "CREATE TABLE IF NOT EXISTS leanpostgres_test_timestamptz (id integer, ts timestamptz)"
  create.exec
  let clear ← prepare conn "DELETE FROM leanpostgres_test_timestamptz"
  clear.exec

  match timestamptzOfText? "2026-07-30 13:05:07.5+05:30" with
  | none => throw <| IO.userError "failed to parse test timestamptz literal"
  | some dt =>
    let insert ← prepare conn "INSERT INTO leanpostgres_test_timestamptz (id, ts) VALUES ($1, $2)"
    insert.bind 1 (1 : Int32)
    insert.bind 2 dt
    insert.exec

    let select ← prepare conn "SELECT ts FROM leanpostgres_test_timestamptz WHERE id = $1"
    select.bind 1 (1 : Int32)
    let hasRow ← select.step
    if !hasRow then throw <| IO.userError "expected a row"
    let result ← ResultColumn.get (α := Std.Time.DateTime) select 0
    -- Compare the absolute instant, not the raw offset, since the session TimeZone (pinned to
    -- UTC above) normalizes the displayed offset to +00 regardless of what was inserted.
    if result.toTimestamp != dt.toTimestamp then
      throw <| IO.userError
        s!"timestamptz round trip failed: expected instant {timestamptzToText dt}, got {timestamptzToText result}"
    if result.timezone.offset.second.val != 0 then
      throw <| IO.userError
        s!"expected timestamptz to normalize to the pinned UTC session TimeZone, got offset {result.timezone.offset.second.val}"
    IO.println s!"Timestamptz round trip OK ({timestamptzToText result}, session TimeZone pinned to UTC)"

/--
Round-trips 1-D arrays, including an empty array, a `NULL` element, and string elements needing
`{a,b,c}`-grammar quoting (comma, space, embedded quote, backslash, and the literal word `NULL`).
-/
def checkArrayRoundTrip (conn : Conn) : IO Unit := do
  let create ← prepare conn
    "CREATE TABLE IF NOT EXISTS leanpostgres_test_array
       (id integer, ints integer[], names text[], maybe_ints integer[])"
  create.exec
  let clear ← prepare conn "DELETE FROM leanpostgres_test_array"
  clear.exec

  let ints : Array Int32 := #[1, -2, 3]
  let names : Array String := #["hello", "a,b", "with space", "she said \"hi\"", "NULL", "", "back\\slash"]
  let maybeInts : Array (Option Int32) := #[some 1, none, some (-3)]

  let insert ← prepare conn
    "INSERT INTO leanpostgres_test_array (id, ints, names, maybe_ints) VALUES ($1, $2, $3, $4)"
  insert.bind 1 (1 : Int32)
  insert.bind 2 ints
  insert.bind 3 names
  insert.bind 4 maybeInts
  insert.exec

  let insertEmpty ← prepare conn
    "INSERT INTO leanpostgres_test_array (id, ints, names, maybe_ints) VALUES ($1, $2, $3, $4)"
  insertEmpty.bind 1 (2 : Int32)
  insertEmpty.bind 2 (#[] : Array Int32)
  insertEmpty.bind 3 (#[] : Array String)
  insertEmpty.bind 4 (#[] : Array (Option Int32))
  insertEmpty.exec

  let select ← prepare conn
    "SELECT ints, names, maybe_ints FROM leanpostgres_test_array WHERE id = $1"
  select.bind 1 (1 : Int32)
  let hasRow ← select.step
  if !hasRow then throw <| IO.userError "expected a row"
  let resultInts ← ResultColumn.get (α := Array Int32) select 0
  let resultNames ← ResultColumn.get (α := Array String) select 1
  let resultMaybeInts ← ResultColumn.get (α := Array (Option Int32)) select 2
  if resultInts != ints then throw <| IO.userError s!"Int32 array round trip failed: {repr resultInts}"
  if resultNames != names then throw <| IO.userError s!"String array round trip failed: {repr resultNames}"
  if resultMaybeInts != maybeInts then
    throw <| IO.userError s!"Option Int32 array round trip failed: {repr resultMaybeInts}"

  let selectEmpty ← prepare conn
    "SELECT ints, names, maybe_ints FROM leanpostgres_test_array WHERE id = $1"
  selectEmpty.bind 1 (2 : Int32)
  let hasRow2 ← selectEmpty.step
  if !hasRow2 then throw <| IO.userError "expected a second row"
  let emptyInts ← ResultColumn.get (α := Array Int32) selectEmpty 0
  let emptyNames ← ResultColumn.get (α := Array String) selectEmpty 1
  let emptyMaybeInts ← ResultColumn.get (α := Array (Option Int32)) selectEmpty 2
  if !emptyInts.isEmpty || !emptyNames.isEmpty || !emptyMaybeInts.isEmpty then
    throw <| IO.userError "expected all three array columns to round-trip as empty"

  IO.println "Array round trip OK (Int32/String/Option Int32, incl. empty array, NULL element, quoting)"

/--
`columnName`/`columnTableName`/`columnOriginName`/`columnDatabaseName`, both unaliased (origin ==
alias) and aliased (origin differs from the alias `columnName` returns).
-/
def checkColumnMetadata (conn : Conn) : IO Unit := do
  let create ← prepare conn
    "CREATE TABLE IF NOT EXISTS leanpostgres_test_metadata
       (user_id integer, user_name text, user_email text)"
  create.exec
  let clear ← prepare conn "DELETE FROM leanpostgres_test_metadata"
  clear.exec

  let select ← prepare conn "SELECT user_id, user_name, user_email FROM leanpostgres_test_metadata"
  discard select.step

  let names ← #[0, 1, 2].mapM (Stmt.columnName select ·)
  if names != #["user_id", "user_name", "user_email"] then
    throw <| IO.userError s!"unexpected column names: {names}"

  let tableNames ← #[0, 1, 2].mapM (Stmt.columnTableName select ·)
  if tableNames != #["leanpostgres_test_metadata", "leanpostgres_test_metadata", "leanpostgres_test_metadata"] then
    throw <| IO.userError s!"unexpected column table names: {tableNames}"

  let originNames ← #[0, 1, 2].mapM (Stmt.columnOriginName select ·)
  if originNames != #["user_id", "user_name", "user_email"] then
    throw <| IO.userError s!"unexpected (unaliased) column origin names: {originNames}"

  let dbName ← select.columnDatabaseName 0
  if dbName.isEmpty then throw <| IO.userError "expected a non-empty database name"

  let selectAliased ← prepare conn
    "SELECT user_id AS id, user_name AS name FROM leanpostgres_test_metadata"
  discard selectAliased.step

  let aliasNames ← #[0, 1].mapM (Stmt.columnName selectAliased ·)
  if aliasNames != #["id", "name"] then throw <| IO.userError s!"unexpected aliased column names: {aliasNames}"

  let aliasOrigins ← #[0, 1].mapM (Stmt.columnOriginName selectAliased ·)
  if aliasOrigins != #["user_id", "user_name"] then
    throw <| IO.userError s!"unexpected aliased column origin names: {aliasOrigins}"

  IO.println s!"Column metadata OK (name/tableName/originName/databaseName={dbName}, incl. aliasing)"

/--
`commandTag`/`commandTuples`/`isReadOnly` across `SELECT`/`INSERT`/`UPDATE`/`DELETE`/`BEGIN`.
-/
def checkCommandMetadata (conn : Conn) : IO Unit := do
  let create ← prepare conn "CREATE TABLE IF NOT EXISTS leanpostgres_test_command (id integer, val text)"
  create.exec
  let clear ← prepare conn "DELETE FROM leanpostgres_test_command"
  clear.exec

  let insert ← prepare conn "INSERT INTO leanpostgres_test_command (id, val) VALUES ($1, $2), ($3, $4)"
  insert.bind 1 (1 : Int32)
  insert.bind 2 "a"
  insert.bind 3 (2 : Int32)
  insert.bind 4 "b"
  discard insert.step
  let insertTag ← insert.commandTag
  let insertTuples ← insert.commandTuples
  let insertReadOnly ← insert.isReadOnly
  if !insertTag.startsWith "INSERT" then throw <| IO.userError s!"unexpected INSERT command tag: {insertTag}"
  if insertTuples != some 2 then throw <| IO.userError s!"unexpected INSERT commandTuples: {insertTuples}"
  if insertReadOnly then throw <| IO.userError "INSERT incorrectly classified as read-only"

  let update ← prepare conn "UPDATE leanpostgres_test_command SET val = $1 WHERE id = $2"
  update.bind 1 "updated"
  update.bind 2 (1 : Int32)
  discard update.step
  let updateTag ← update.commandTag
  let updateTuples ← update.commandTuples
  if updateTag != "UPDATE 1" then throw <| IO.userError s!"unexpected UPDATE command tag: {updateTag}"
  if updateTuples != some 1 then throw <| IO.userError s!"unexpected UPDATE commandTuples: {updateTuples}"

  let delete ← prepare conn "DELETE FROM leanpostgres_test_command WHERE id = $1"
  delete.bind 1 (2 : Int32)
  discard delete.step
  let deleteTuples ← delete.commandTuples
  if deleteTuples != some 1 then throw <| IO.userError s!"unexpected DELETE commandTuples: {deleteTuples}"

  let select ← prepare conn "SELECT * FROM leanpostgres_test_command"
  discard select.step
  let selectTag ← select.commandTag
  let selectTuples ← select.commandTuples
  let selectReadOnly ← select.isReadOnly
  -- one row survives the insert-2/update-1/delete-1 sequence above.
  if !selectTag.startsWith "SELECT" then throw <| IO.userError s!"unexpected SELECT command tag: {selectTag}"
  if selectTuples != some 1 then throw <| IO.userError s!"unexpected SELECT commandTuples: {selectTuples}"
  if !selectReadOnly then throw <| IO.userError "SELECT incorrectly classified as not read-only"

  let begin_ ← prepare conn "BEGIN"
  discard begin_.step
  let beginReadOnly ← begin_.isReadOnly
  if !beginReadOnly then throw <| IO.userError "BEGIN incorrectly classified as not read-only"
  (← prepare conn "ROLLBACK").exec

  IO.println "Command metadata OK (commandTag/commandTuples/isReadOnly across SELECT/INSERT/UPDATE/DELETE/BEGIN)"

def main : IO Unit := do
  checkFFIInitialized
  checkConnectSuccess
  checkConnectFailure
  let conn ← «open» ""
  checkParameterizedRoundTrip conn
  checkEmptyResultSet conn
  checkNonSelectCommands conn
  checkMalformedStatementError conn
  checkCoreTypeRoundTrip conn
  checkTupleRowIteration conn
  checkInterpolationMacros conn
  checkTransactionOptionsCombinations conn
  checkTransactionCommitAndRollback conn
  checkUniqueViolationSqlstate conn
  checkPersonRowDeriving conn
  checkNullablePersonRowDeriving conn
  checkProductRowDeriving conn
  checkAllOptionalRowDeriving conn
  checkEmptyRowDeriving conn
  checkWrapperTypeDeriving conn
  checkCoordinateRowDeriving conn
  checkEmailDeriving conn
  checkNonEmptyStringQueryParamDeriving conn
  checkBlobDeriving
  checkNumericRoundTrip conn
  checkUuidRoundTrip conn
  checkDateRoundTrip conn
  checkTimeRoundTrip conn
  checkTimestampRoundTrip conn
  checkTimestamptzRoundTrip conn
  checkArrayRoundTrip conn
  checkColumnMetadata conn
  checkCommandMetadata conn
  IO.println "all checks passed"
