/-
Copyright (c) 2026 Paul Butcher. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Postgres
import PostgresTest.Framework
import Tests.Helpers

open Postgres
open Postgres.Interpolation
open Postgres.Test

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
def testPersonRowDeriving (conn : Conn) : TestM Unit :=
  withHeader "=== Testing Person Row deriving ===" <| withRollback conn <| guardTest do
    conn exec!"CREATE TABLE IF NOT EXISTS leanpostgres_test_people (name text NOT NULL, age integer NOT NULL)"
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
    recordSuccess s!"Person Row deriving OK: {repr people}"

/-! ## NullablePerson: `Option` field handling -/

structure NullablePerson where
  name : String
  nickname : Option String
  age : Int32
deriving Repr, BEq, Row

/-- `Row` deriving handles `Option` fields (`NULL`) correctly. -/
def testNullablePersonRowDeriving (conn : Conn) : TestM Unit :=
  withHeader "=== Testing NullablePerson Row deriving ===" <| withRollback conn <| guardTest do
    conn exec!"CREATE TABLE IF NOT EXISTS leanpostgres_test_nullable_people
                 (name text NOT NULL, nickname text, age integer NOT NULL)"
    conn exec!"INSERT INTO leanpostgres_test_nullable_people (name, nickname, age) VALUES ('Alice', 'Ali', 30)"
    conn exec!"INSERT INTO leanpostgres_test_nullable_people (name, nickname, age) VALUES ('Bob', NULL, 25)"

    let select ← prepare conn "SELECT name, nickname, age FROM leanpostgres_test_nullable_people ORDER BY name"
    let people ← (select.resultsAs NullablePerson).toArray
    let expected : Array NullablePerson :=
      #[{ name := "Alice", nickname := some "Ali", age := 30 }, { name := "Bob", nickname := none, age := 25 }]
    if people != expected then throw <| IO.userError s!"expected {repr expected}, got {repr people}"
    recordSuccess s!"NullablePerson Row deriving OK: {repr people}"

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
def testProductRowDeriving (conn : Conn) : TestM Unit :=
  withHeader "=== Testing Product Row deriving ===" <| withRollback conn <| guardTest do
    conn exec!"CREATE TABLE IF NOT EXISTS leanpostgres_test_products
                 (id bigint NOT NULL, name text NOT NULL, price double precision NOT NULL, in_stock boolean NOT NULL)"
    conn exec!"INSERT INTO leanpostgres_test_products (id, name, price, in_stock) VALUES (1, 'Widget', 19.99, true)"
    conn exec!"INSERT INTO leanpostgres_test_products (id, name, price, in_stock) VALUES (2, 'Gadget', 29.99, false)"

    let select ← prepare conn "SELECT id, name, price, in_stock FROM leanpostgres_test_products ORDER BY id"
    let products ← (select.resultsAs Product).toArray
    let expected : Array Product :=
      #[{ id := 1, name := "Widget", price := 19.99, inStock := true },
        { id := 2, name := "Gadget", price := 29.99, inStock := false }]
    if products != expected then throw <| IO.userError s!"expected {repr expected}, got {repr products}"
    recordSuccess s!"Product Row deriving OK: {repr products}"

/-! ## AllOptional: every field is `Option` -/

structure AllOptional where
  a : Option String
  b : Option Int32
  c : Option Float
deriving Repr, Inhabited, Row

instance : BEq AllOptional where
  beq x y := x.a == y.a && x.b == y.b && optApproxEq x.c y.c

/-- `Row` deriving when every field is nullable. -/
def testAllOptionalRowDeriving (conn : Conn) : TestM Unit :=
  withHeader "=== Testing AllOptional Row deriving ===" <| withRollback conn <| guardTest do
    conn exec!"CREATE TABLE IF NOT EXISTS leanpostgres_test_all_optional (a text, b integer, c double precision)"
    conn exec!"INSERT INTO leanpostgres_test_all_optional (a, b, c) VALUES ('test', 42, 3.14)"
    conn exec!"INSERT INTO leanpostgres_test_all_optional (a, b, c) VALUES (NULL, NULL, NULL)"

    let select ← prepare conn "SELECT a, b, c FROM leanpostgres_test_all_optional ORDER BY a NULLS LAST"
    let rows ← (select.resultsAs AllOptional).toArray
    let expected : Array AllOptional :=
      #[{ a := some "test", b := some 42, c := some 3.14 }, { a := none, b := none, c := none }]
    if rows != expected then throw <| IO.userError s!"expected {repr expected}, got {repr rows}"
    recordSuccess s!"AllOptional Row deriving OK: {repr rows}"

/-! ## EmptyRow: a zero-field structure -/

structure EmptyRow where
deriving Repr, BEq, Row

/-- `Row` deriving for a zero-field structure reads no columns, one `EmptyRow` per row. -/
def testEmptyRowDeriving (conn : Conn) : TestM Unit :=
  withHeader "=== Testing EmptyRow Row deriving ===" <| withRollback conn <| guardTest do
    conn exec!"CREATE TABLE IF NOT EXISTS leanpostgres_test_empty_row (dummy integer)"
    conn exec!"INSERT INTO leanpostgres_test_empty_row (dummy) VALUES (1)"
    conn exec!"INSERT INTO leanpostgres_test_empty_row (dummy) VALUES (2)"
    conn exec!"INSERT INTO leanpostgres_test_empty_row (dummy) VALUES (3)"

    let select ← prepare conn "SELECT 1 FROM leanpostgres_test_empty_row"
    let empties ← (select.resultsAs EmptyRow).toArray
    if empties.size != 3 then throw <| IO.userError s!"expected 3 empty rows, got {empties.size}"
    recordSuccess s!"EmptyRow Row deriving OK: {empties.size} rows"

/-! ## UserId/Username: trivial wrappers with `ResultColumn`/`QueryParam` -/

structure UserId where
  id : Int64
deriving Repr, BEq, Inhabited, ResultColumn, QueryParam

structure Username where
  name : String
deriving Repr, BEq, Inhabited, ResultColumn, QueryParam

/-- `ResultColumn`/`QueryParam` deriving for trivial single-field wrapper types. -/
def testWrapperTypeDeriving (conn : Conn) : TestM Unit :=
  withHeader "=== Testing UserId/Username ResultColumn/QueryParam deriving ===" <| withRollback conn <| guardTest do
    conn exec!"CREATE TABLE IF NOT EXISTS leanpostgres_test_wrapper_users (id bigint NOT NULL, username text NOT NULL)"
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

    recordSuccess "UserId/Username ResultColumn/QueryParam deriving OK"

/-! ## Coordinate/Email: non-structure inductive types -/

inductive Coordinate where
  | mk (x : Float) (y : Float)
deriving Repr, BEq, Inhabited, Row

/-- `Row` deriving works for a non-structure (single-constructor) inductive type. -/
def testCoordinateRowDeriving (conn : Conn) : TestM Unit :=
  withHeader "=== Testing Coordinate Row deriving ===" <| withRollback conn <| guardTest do
    conn exec!"CREATE TABLE IF NOT EXISTS leanpostgres_test_coordinates (x double precision NOT NULL, y double precision NOT NULL)"
    conn exec!"INSERT INTO leanpostgres_test_coordinates (x, y) VALUES (1.0, 2.0)"
    conn exec!"INSERT INTO leanpostgres_test_coordinates (x, y) VALUES (3.5, 4.5)"

    let select ← prepare conn "SELECT x, y FROM leanpostgres_test_coordinates ORDER BY x"
    let coords ← (select.resultsAs Coordinate).toArray
    let expected : Array Coordinate := #[.mk 1.0 2.0, .mk 3.5 4.5]
    if coords != expected then throw <| IO.userError s!"expected {repr expected}, got {repr coords}"
    recordSuccess s!"Coordinate Row deriving OK: {repr coords}"

inductive Email where
  | mk (addr : String)
deriving Repr, BEq, Inhabited, ResultColumn, QueryParam

/-- `ResultColumn`/`QueryParam` deriving works for a non-structure (single-field) inductive type. -/
def testEmailDeriving (conn : Conn) : TestM Unit :=
  withHeader "=== Testing Email ResultColumn/QueryParam deriving ===" <| withRollback conn <| guardTest do
    conn exec!"CREATE TABLE IF NOT EXISTS leanpostgres_test_emails (addr text NOT NULL)"
    conn exec!"INSERT INTO leanpostgres_test_emails (addr) VALUES ('alice@example.com')"
    conn exec!"INSERT INTO leanpostgres_test_emails (addr) VALUES ('bob@example.com')"

    let select ← prepare conn "SELECT addr FROM leanpostgres_test_emails ORDER BY addr"
    let emails ← (select.resultsAs Email).toArray
    let expected : Array Email := #[.mk "alice@example.com", .mk "bob@example.com"]
    if emails != expected then throw <| IO.userError s!"expected {repr expected}, got {repr emails}"

    let target : Email := .mk "bob@example.com"
    let byAddr ← (← conn query!"SELECT addr FROM leanpostgres_test_emails WHERE addr = {target}" as Email).toArray
    if byAddr != #[Email.mk "bob@example.com"] then throw <| IO.userError s!"expected bob's email, got {repr byAddr}"
    recordSuccess s!"Email ResultColumn/QueryParam deriving OK: {repr emails}"

/-! ## NonEmptyString: `QueryParam` ignores proof fields -/

structure NonEmptyString where
  val : String
  nonEmpty : val.length > 0
deriving QueryParam

/-- `QueryParam` deriving ignores proof fields; only `val` is bound. -/
def testNonEmptyStringQueryParamDeriving (conn : Conn) : TestM Unit :=
  withHeader "=== Testing NonEmptyString QueryParam deriving ===" <| withRollback conn <| guardTest do
    conn exec!"CREATE TABLE IF NOT EXISTS leanpostgres_test_nonempty_strings (val text NOT NULL)"
    conn exec!"INSERT INTO leanpostgres_test_nonempty_strings (val) VALUES ('hello')"
    conn exec!"INSERT INTO leanpostgres_test_nonempty_strings (val) VALUES ('world')"

    let target : NonEmptyString := ⟨"hello", by decide⟩
    let hits ← (← conn query!"SELECT val FROM leanpostgres_test_nonempty_strings WHERE val = {target}" as String).toArray
    if hits != #["hello"] then throw <| IO.userError s!"expected hello, got {hits}"
    recordSuccess "NonEmptyString QueryParam deriving (proof field ignored) OK"

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
