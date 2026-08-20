/-
Copyright (c) 2026 Paul Butcher. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Postgres
import PostgresTest.Framework
import Plausible

open Postgres.Blob
open Postgres.Test
open Plausible

private def genLeaf : Gen Json :=
  Gen.oneOf #[
    pure .null,
    .bool <$> Arbitrary.arbitrary,
    (fun (m, e) => .num ⟨m, e⟩) <$> Gen.prodOf Arbitrary.arbitrary Arbitrary.arbitrary,
    .str <$> Arbitrary.arbitrary
  ]

/--
Generates a `Json` of at most `depth` nesting. Leaves outweigh containers and a container has at
most three children, which keeps the expected number of nodes small however deep the bound is.
-/
private def genJson : Nat → Gen Json
  | 0 => genLeaf
  | depth + 1 =>
    Gen.frequency genLeaf [
      (2, genLeaf),
      (1, do
        let width ← Gen.choose Nat 0 3 (by omega)
        .arr <$> (Array.range width).mapM fun _ => genJson depth),
      (1, do
        let width ← Gen.choose Nat 0 3 (by omega)
        .obj <$> (Array.range width).mapM fun _ => do
          return (← Arbitrary.arbitrary, ← genJson depth))
    ]

instance : Arbitrary Json where
  arbitrary := Gen.sized fun size => genJson (min size 6)

instance : Shrinkable Json where
  shrink
    | .arr elems => elems.toList
    | .obj fields => fields.toList.map (·.2)
    | _ => []

private structure Point where
  x : Int
  y : Int
deriving BEq, Repr

private instance : Json.ToJson Point := ⟨fun p => Json.toJson (p.x, p.y)⟩

private instance : Json.FromJson Point := ⟨fun j => do
  let (x, y) ← Json.fromJson? j
  return { x, y }⟩

private instance : ToBinary Point := .viaJson

private instance : FromBinary Point := .viaJson

private def roundTrips [ToBinary α] [FromBinary α] [BEq α] (x : α) : Bool :=
  match fromBinary (toBinary x) with
  | .ok y => x == y
  | .error _ => false

/--
An array nested `depth` deep, the shape whose cost the two codecs each have to keep off the C stack.
-/
private def nested (depth : Nat) : Json :=
  depth.fold (init := .null) fun _ _ j => .arr #[j]

/--
The `Json` codecs, which are written by hand rather than derived: every kind of node, the object
field order and duplicate names that this `Json` can represent and a sorted map cannot, and
`viaJson`.
-/
def testBlobJson : TestM Unit :=
  withHeader "=== Testing Blob Json codecs ===" <| guardTest do
    let checks : List Bool := [
      roundTrips (Json.null),
      roundTrips (Json.bool true), roundTrips (Json.bool false),
      roundTrips (Json.num ⟨-125, 7⟩),
      roundTrips (Json.str "quoth the raven \"nevermore\""),
      roundTrips (Json.arr #[]), roundTrips (Json.obj #[]),
      roundTrips (Json.obj #[("b", .num 1), ("a", .num 2), ("b", .null)]),
      roundTrips (Json.arr #[.obj #[("xs", .arr #[.null, .bool true])], .str ""]),
      roundTrips (nested 100000),
      roundTrips ({ x := 3, y := -4 } : Point)
    ]
    if !checks.all id then
      throw <| IO.userError s!"expected every Json round trip to succeed, got {checks}"

    -- Field order and duplicates survive the round trip, rather than being sorted or merged.
    let duplicates := Json.obj #[("b", .num 1), ("a", .num 2), ("b", .null)]
    match fromBinaryOf Json (toBinary duplicates) with
    | .ok (.obj fields) =>
      if fields.map (·.1) != #["b", "a", "b"] then
        throw <| IO.userError s!"object field order not preserved: {fields.map (·.1)}"
    | other => throw <| IO.userError s!"expected an object back, got {repr other}"

    match fromBinaryOf Json ⟨#[7]⟩ with
    | .error msg =>
      if msg != "Expected tag 0-6 for `Json`, got 7" then
        throw <| IO.userError s!"unexpected error message for an unknown tag: {msg}"
    | .ok _ => throw <| IO.userError "expected an unknown tag to be rejected"

    recordSuccess "Blob Json codecs OK"

/--
A property rather than a theorem. `fromBinary (toBinary j) = .ok j` is the claim worth proving, but
it bottoms out in the `ByteArray` and UTF-8 primitives the kernel will not reduce (as described in
`Tests.CodecProperties`), and above that it would have to relate `Json.Alg.fold`, whose recursion
is over a work list, to the decoder's, which is over a fuel bound: two different recursions with no
shared induction principle to state the step of the proof against.
-/
def testBlobJsonProperty : TestM Unit :=
  withHeader "=== Testing Blob Json codecs (property-based) ===" <| guardTest do
    let property := NamedBinder "j" (∀ j : Json, roundTrips j)
    match ← Testable.checkIO property { maxSize := 500 } with
    | .success _ => recordSuccess "Json: Blob round trip held across generated examples"
    | .gaveUp n => throw <| IO.userError s!"Json: gave up after {n} attempts"
    | .failure _ xs n =>
      throw <| IO.userError (Testable.formatFailure "Json: found a counter-example" xs n)
