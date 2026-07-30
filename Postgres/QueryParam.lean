module
import all Postgres.TextCodec
public import Postgres.TextCodec
public import Postgres.LowLevel

set_option doc.verso true
set_option linter.missingDocs true

namespace Postgres

public section

/--
Provides a canonical means of binding a non-null value of type {name}`α` as a Postgres query
parameter.
-/
public class QueryParam (α : Type u) where
  /-- Binds {name}`α` at the (1-based) parameter index. -/
  bind : Stmt → Int32 → α → IO Unit

public instance : QueryParam String where
  bind := Stmt.bindText

public instance : QueryParam Bool where
  bind stmt i b := stmt.bindText i (if b then "t" else "f")

public instance : QueryParam Int16 where
  bind stmt i n := stmt.bindText i (toString n)

public instance : QueryParam Int32 where
  bind stmt i n := stmt.bindText i (toString n)

public instance : QueryParam Int64 where
  bind stmt i n := stmt.bindText i (toString n)

/-- Binds a {lit}`real` parameter at (1-based) {name}`index`. -/
public def Stmt.bindFloat32 (stmt : Stmt) (index : Int32) (value : Float32) : IO Unit :=
  stmt.bindText index (TextCodec.floatToText value.toFloat)

/-- Binds a {lit}`double precision` parameter at (1-based) {name}`index`. -/
public def Stmt.bindFloat (stmt : Stmt) (index : Int32) (value : Float) : IO Unit :=
  stmt.bindText index (TextCodec.floatToText value)

/-- Binds a {lit}`bytea` parameter at (1-based) {name}`index`, hex-encoded. -/
public def Stmt.bindBytes (stmt : Stmt) (index : Int32) (value : ByteArray) : IO Unit :=
  stmt.bindText index (TextCodec.byteArrayToHex value)

public instance : QueryParam Float32 where
  bind := Stmt.bindFloat32

public instance : QueryParam Float where
  bind := Stmt.bindFloat

public instance : QueryParam ByteArray where
  bind := Stmt.bindBytes

public instance : QueryParam Unit where
  bind stmt i _ := stmt.bindNull i

/--
Provides a canonical means of binding a potentially-null value of type {name}`α` as a Postgres
query parameter.
-/
public class NullableQueryParam (α : Type u) where
  /-- Binds {name}`α` at the (1-based) parameter index. -/
  bind : Stmt → Int32 → α → IO Unit

public instance [QueryParam α] : NullableQueryParam α where
  bind := QueryParam.bind

public instance [QueryParam α] : NullableQueryParam (Option α) where
  bind stmt i
    | none => stmt.bindNull i
    | some val => QueryParam.bind stmt i val

/--
Binds a query parameter based on its Lean type's {name}`NullableQueryParam` instance.

{name}`index` is the 1-based positional index of the parameter to be bound.
-/
def Stmt.bind [NullableQueryParam α] (stmt : Stmt) (index : Int32) (param : α) : IO Unit :=
  NullableQueryParam.bind stmt index param

end
end Postgres
