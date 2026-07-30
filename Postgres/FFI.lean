/-!
Raw FFI bindings to libpq (`bindings/leanpostgres.c`): opaque external types only.
Higher-level wrapping lives in `Postgres.LowLevel`.
-/

namespace Postgres.FFI

opaque ConnPointed : NonemptyType
/-- Opaque wrapper around a `PGconn*`. -/
def Conn : Type := ConnPointed.type
instance : Nonempty Conn := ConnPointed.property

opaque ResultPointed : NonemptyType
/-- Opaque wrapper around a `PGresult*`. -/
def Result : Type := ResultPointed.type
instance : Nonempty Result := ResultPointed.property

@[extern "leanpostgres_initialize"]
private opaque initModule : IO Unit

initialize initModule

end Postgres.FFI
