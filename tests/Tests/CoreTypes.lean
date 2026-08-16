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

/--
Round-trips every scalar core type through `QueryParam`/`ResultColumn`, including bound and read
`NULL` (`Option α`).
-/
def testCoreTypeRoundTrip (conn : Conn) : TestM Unit :=
  withHeader "=== Testing core type round trip ===" <| withRollback conn <| guardTest do
    let create ← prepare conn
      "CREATE TABLE IF NOT EXISTS leanpostgres_test_types
         (flag boolean, small smallint, n integer, big bigint,
          r real, d double precision, t text, b bytea)"
    create.exec

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

    recordSuccess "core type round trip OK (Bool/Int16/Int32/Int64/Float32/Float/String/ByteArray, incl. NULL)"

/--
Uses the tuple `Row` instance and `Stmt.resultsAs` to read a 3-column table and iterate a
multi-row result to completion.
-/
def testTupleRowIteration (conn : Conn) : TestM Unit :=
  withHeader "=== Testing tuple Row + multi-row iteration ===" <| withRollback conn <| guardTest do
    let create ← prepare conn
      "CREATE TABLE IF NOT EXISTS leanpostgres_test_rows (id integer, name text, active boolean)"
    create.exec

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
    recordSuccess s!"tuple Row + multi-row iteration OK: {rows}"

/-- Exercises `exec!`/`query!` with a mix of literal SQL and interpolated parameters. -/
def testInterpolationMacros (conn : Conn) : TestM Unit :=
  withHeader "=== Testing interpolation macros ===" <| withRollback conn <| guardTest do
    conn exec!"CREATE TABLE IF NOT EXISTS leanpostgres_test_interp (id integer, name text, active boolean)"

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
    recordSuccess s!"interpolation macros OK: {collected}"
