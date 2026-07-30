module
import all Postgres.TextCodec
public import Postgres.TextCodec
public import Postgres.LowLevel
import Std.Data.Iterators

set_option doc.verso true
set_option linter.missingDocs true

namespace Postgres

public section

/--
Provides a canonical way to read a non-null value of type {name}`α` from a Postgres query
result column.
-/
public class ResultColumn (α : Type) where
  /-- Reads the (0-based) {name}`column` of the current row as an {name}`α`. -/
  get (stmt : Stmt) (column : Int32) : IO α

/--
Reads a nullable column, checking {name (full := Stmt.columnIsNull)}`columnIsNull` directly
rather than consulting the column's Postgres type (OID) — {name}`ResultColumn` instances trust
the caller's declared Lean type, not the schema, so there is no OID to fall back on here either.
-/
public instance [ResultColumn α] : ResultColumn (Option α) where
  get stmt i := do
    if ← stmt.columnIsNull i then
      return none
    else
      some <$> ResultColumn.get stmt i

public instance : ResultColumn String where
  get := Stmt.columnText

public instance : ResultColumn Bool where
  get stmt i := do
    match ← stmt.columnText i with
    | "t" => return true
    | "f" => return false
    | s => throw <| IO.userError s!"column {i}: expected a boolean ('t'/'f'), got: {s}"

private def parseChecked (name : String) (lo hi : Int) (stmt : Stmt) (i : Int32) : IO Int := do
  let s ← stmt.columnText i
  match s.toInt? with
  | some n =>
    if lo ≤ n && n ≤ hi then return n
    else throw <| IO.userError s!"column {i}: {n} is out of range for {name}"
  | none => throw <| IO.userError s!"column {i}: expected an integer, got: {s}"

/-- Reads a {lit}`smallint` column at (0-based) {name}`column`. -/
public def Stmt.columnInt16 (stmt : Stmt) (column : Int32) : IO Int16 := do
  return Int16.ofInt (← parseChecked "Int16" Int16.minValue.toInt Int16.maxValue.toInt stmt column)

/-- Reads an {lit}`integer` column at (0-based) {name}`column`. -/
public def Stmt.columnInt32 (stmt : Stmt) (column : Int32) : IO Int32 := do
  return Int32.ofInt (← parseChecked "Int32" Int32.minValue.toInt Int32.maxValue.toInt stmt column)

/-- Reads a {lit}`bigint` column at (0-based) {name}`column`. -/
public def Stmt.columnInt64 (stmt : Stmt) (column : Int32) : IO Int64 := do
  return Int64.ofInt (← parseChecked "Int64" Int64.minValue.toInt Int64.maxValue.toInt stmt column)

/-- Reads a {lit}`real` column at (0-based) {name}`column`. -/
public def Stmt.columnFloat32 (stmt : Stmt) (column : Int32) : IO Float32 := do
  let s ← stmt.columnText column
  match TextCodec.floatOfText? s with
  | some f => return f.toFloat32
  | none => throw <| IO.userError s!"column {column}: expected a float, got: {s}"

/-- Reads a {lit}`double precision` column at (0-based) {name}`column`. -/
public def Stmt.columnFloat (stmt : Stmt) (column : Int32) : IO Float := do
  let s ← stmt.columnText column
  match TextCodec.floatOfText? s with
  | some f => return f
  | none => throw <| IO.userError s!"column {column}: expected a float, got: {s}"

/-- Reads a {lit}`bytea` column at (0-based) {name}`column`, decoding its hex literal. -/
public def Stmt.columnBytes (stmt : Stmt) (column : Int32) : IO ByteArray := do
  let s ← stmt.columnText column
  match TextCodec.hexToByteArray? s with
  | some bytes => return bytes
  | none => throw <| IO.userError s!"column {column}: expected a bytea hex literal, got: {s}"

public instance : ResultColumn Int16 where
  get := Stmt.columnInt16

public instance : ResultColumn Int32 where
  get := Stmt.columnInt32

public instance : ResultColumn Int64 where
  get := Stmt.columnInt64

public instance : ResultColumn Float32 where
  get := Stmt.columnFloat32

public instance : ResultColumn Float where
  get := Stmt.columnFloat

public instance : ResultColumn ByteArray where
  get := Stmt.columnBytes

/--
Provides a canonical way to read a potentially-null value of type {name}`α` from a Postgres
query result column.
-/
public class NullableResultColumn (α : Type) where
  /-- Reads the (0-based) {name}`column` of the current row as an {name}`α`. -/
  get (stmt : Stmt) (column : Int32) : IO α

public instance [ResultColumn α] : NullableResultColumn α where
  get := ResultColumn.get

public instance [ResultColumn α] : NullableResultColumn (Option α) where
  get stmt i := do
    if ← stmt.columnIsNull i then
      return none
    else
      some <$> ResultColumn.get stmt i

/-- A monad for reading a row of data from a query result. -/
public abbrev RowReader (α : Type) := ReaderT Stmt (StateRefT Int32 IO) α

namespace RowReader

/-- Reads the next column using the {name}`ResultColumn` instance for {name}`α`, updating the column counter. -/
public def field [ResultColumn α] : RowReader α := do
  let v ← ResultColumn.get (← read) (← getThe Int32)
  modify (· + 1)
  return v

/--
Runs a row reader on a prepared statement.

The statement should have just been stepped, and returned {lean}`true`.
-/
public def run (stmt : Stmt) (act : RowReader α) : IO α := do
  return (← ReaderT.run act stmt |>.run 0).1

end RowReader

/--
A type that can be deserialized from a complete row of a Postgres query result.

Each field is read sequentially using its {name}`ResultColumn` instance, matching column order.
-/
public class Row (α : Type) where
  /-- Reads a complete row as an {name}`α`. -/
  read : RowReader α

public instance [ResultColumn α] : Row α where
  read := RowReader.field

public instance [Row α] [Row β] : Row (α × β) where
  read := do return (← Row.read, ← Row.read)

/-- An iterator over query results. Stepping the iterator steps the underlying prepared statement. -/
public structure QueryIterator (β : Type) where
  /-- The {name}`Row` instance used to decode each row as a {name}`β`. -/
  [isRow : Row β]
  /-- The statement being iterated. -/
  stmt : Stmt

open Std
open Iterators

public instance : Iterator (QueryIterator β) IO β where
  IsPlausibleStep _it
    | .yield _it' _v => True
    | .done => True
    | .skip .. => False
  step it := do
    match it.internalState with
    | { isRow, stmt } =>
      let hasRows ← stmt.step
      if hasRows then
        return .deflate <| .yield { it with internalState := { isRow, stmt } } (← isRow.read.run stmt) ⟨⟩
      else
        return .deflate <| .done ⟨⟩

public instance [Monad n] : IteratorLoop (QueryIterator β) IO n := IteratorLoop.defaultImplementation

/-- Returns an iterator into all the results of a prepared statement. -/
public def Stmt.results [Row α] (stmt : Stmt) : @IterM (QueryIterator α) IO α where
  internalState := QueryIterator.mk stmt

/-- Returns an iterator into all the results of a prepared statement, with the type specified explicitly. -/
public def Stmt.resultsAs (α : Type) [Row α] (stmt : Stmt) : @IterM (QueryIterator α) IO α := stmt.results

end
end Postgres
