module

public import Postgres.LowLevel
public import Postgres.QueryParam
public import Postgres.QueryResult

set_option doc.verso true
set_option linter.missingDocs true

namespace Postgres
namespace Interpolation

open Lean

/--
An interpolated query that creates a prepared statement.

Accepts only a single SQL statement. Interpolated values are bound as query parameters via their
{name}`NullableQueryParam` instance.
-/
syntax term "sql!" interpolatedStr(term) : term

/-- An interpolated query that executes a single statement, discarding the results. -/
syntax term "exec!" interpolatedStr(term) : term

/--
An interpolated query that creates an iterator into the results.

Accepts only a single SQL statement that returns data.
-/
syntax term "query!" interpolatedStr(term) (&" as " term)? : term

macro_rules
  | `($db sql!%$tk $s) => do
    let chunks := s.raw.getArgs
    let mut sqlStx ← `("")
    let mut args := #[]
    let stmtName := mkIdentFrom tk `stmt
    for c in chunks do
      if let some lit := c.isInterpolatedStrLit? then
        sqlStx ← `($sqlStx ++ $(quote lit))
      else
        let index := args.size + 1
        args := args.push (← ``(NullableQueryParam.bind $stmtName (Nat.toInt32 $(quote index)) $(⟨c⟩)))
        sqlStx ← `($sqlStx ++ $(quote s!"${index}"))

    let stx ← `(((let db := $db; Postgres.prepare db $sqlStx >>= fun $stmtName =>
        (([$(args),*] : List (IO Unit)).forM id) *>
        return $stmtName) : IO Stmt))

    return stx
  | `($db exec!%$tk $s) => do
    `(($db sql!%$tk $s >>= Stmt.exec : IO Unit))
  | `($db query!%$tk $s) => do
    `($db sql!%$tk $s <&> Stmt.results)
  | `($db query!%$tk $s as $ty) => do
    `($db sql!%$tk $s <&> Stmt.resultsAs $ty)

end Interpolation
end Postgres
