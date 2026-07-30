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
