/-
Copyright (c) 2026 Paul Butcher. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module

import Postgres

open Postgres Postgres.Blob

structure Widget where
  name : String
  size : Int32
  note : Option String
deriving Row

structure Label where
  text : String
deriving ResultColumn, QueryParam, ToBinary, FromBinary, BEq, Repr

inductive Shape where
  | circle (radius : Int)
  | rect (width height : Int)
deriving ToBinary, FromBinary, BEq, Repr

structure Point where
  x : Int
  y : Int
deriving BEq, Repr

instance : Json.ToJson Point := ⟨fun p => Json.toJson (p.x, p.y)⟩

instance : Json.FromJson Point := ⟨fun j => do
  let (x, y) ← Json.fromJson? j
  return { x, y }⟩

instance : ToBinary Point := .viaJson

instance : FromBinary Point := .viaJson

-- Running the derived `Row`, `ResultColumn` and `QueryParam` instances needs a live connection, so
-- what is checked here is that they resolve and that their code reaches the executable.
def widgetReader : RowReader Widget := Row.read

def labelColumn : Stmt → Int32 → IO Label := ResultColumn.get

def labelParam : Stmt → Int32 → Label → IO Unit := QueryParam.bind

private def roundTrip [BEq α] [Repr α] [ToBinary α] [FromBinary α] (what : String) (x : α) :
    IO Unit := do
  match fromBinary (toBinary x) with
  | .ok y => unless y == x do throw <| IO.userError s!"{what}: {repr y} ≠ {repr x}"
  | .error e => throw <| IO.userError s!"{what}: {e}"

public def main : IO Unit := do
  roundTrip "Label" ({ text := "urgent" } : Label)
  roundTrip "Shape" (Shape.rect 2 3)
  roundTrip "Point" ({ x := 1, y := -2 } : Point)
  roundTrip "Json" <|
    Json.obj #[("a", .arr #[.num (.ofNat 1), .null, .bool true]), ("b", .str "two")]
  IO.println "linkage: derived instances and JSON codecs round-tripped"
