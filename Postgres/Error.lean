module

set_option doc.verso true
set_option linter.missingDocs true

namespace Postgres

public section

/--
A Postgres error: the five-character SQLSTATE code alongside a human-readable message.

{name (full := Error.sqlstate)}`sqlstate` is empty for failures that occur before a connection
exists (e.g. a refused TCP connection), since libpq has no result object to read a SQLSTATE from
at that point. Once a connection is open, errors from executed statements carry a real SQLSTATE.
-/
structure Error where
  /-- The five-character SQLSTATE code, or the empty string when none is available. -/
  sqlstate : String
  /-- A human-readable description of the error. -/
  message : String
deriving Repr, BEq, Inhabited

namespace Error

instance : ToString Error where
  toString e := s!"[{e.sqlstate}] {e.message}"

/--
Recovers the {name}`Error` carried by an {name (full := IO.Error)}`IO.Error`, if it was thrown
by this library (identified by {name}`ToString.toString`'s {lit}`[sqlstate] message` format).
Returns {lean}`none` for any other {name (full := IO.Error)}`IO.Error`, including ones from
unrelated {name}`IO` actions.
-/
def ofIOError? : IO.Error → Option Error
  | .userError msg =>
    if msg.startsWith "[" then
      match msg.splitOn "] " with
      | code :: (rest@(_ :: _)) =>
        some { sqlstate := (code.drop 1).toString, message := String.intercalate "] " rest }
      | _ => none
    else none
  | _ => none

end Error

end
end Postgres
