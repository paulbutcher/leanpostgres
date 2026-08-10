/-
Copyright (c) 2026 Paul Butcher. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Postgres.Blob.Deriving
import PostgresTest.Framework
import Plausible

open Postgres.Blob
open Postgres.Test
open Plausible

structure Pair where
  x : Nat
  y : String
deriving BEq, Repr, ToBinary, FromBinary, Arbitrary, Shrinkable

inductive Color where
  | r | g | b
deriving BEq, Repr, Inhabited, ToBinary, FromBinary, Arbitrary, Shrinkable

inductive Shape where
  | circle (radius : Nat)
  | rect (w : Nat) (h : Nat)
deriving BEq, Repr, ToBinary, FromBinary, Arbitrary, Shrinkable

inductive Msg where
  | flagged (b : Bool) (s : String)
  | plain (s : String)
deriving BEq, Repr, ToBinary, FromBinary, Arbitrary, Shrinkable

inductive Cmd where
  | exec (retries : Option Nat) (cmd : String)
  | noop
deriving BEq, Repr, ToBinary, FromBinary, Arbitrary, Shrinkable

structure Box (α : Type) where
  val : α
deriving BEq, Repr, ToBinary, FromBinary, Arbitrary, Shrinkable

inductive Tree where
  | leaf (val : Nat)
  | node (left : Tree) (right : Tree)
deriving BEq, Repr, ToBinary, FromBinary

/--
`Tree`'s two-child `node` constructor makes Plausible's derived `Arbitrary` unusable: the deriving
handler decrements fuel by exactly 1 per recursive call but doesn't split it between the two
children, so both children recurse with the *same* fuel and the expected node count grows
exponentially in the fuel (i.e. the size parameter). Halving the fuel for each child here keeps
generated trees' size linear in the size parameter, matching the deriving handler's own convention
(constructor weights following the QuickChick convention: 1 for `leaf`, remaining fuel for `node`)
everywhere else.
-/
def Tree.arbitraryGo : Nat → Gen Tree
  | 0 => Tree.leaf <$> Arbitrary.arbitrary
  | n + 1 => Gen.frequency (Tree.leaf <$> Arbitrary.arbitrary) [
      (1, Tree.leaf <$> Arbitrary.arbitrary),
      (n, do
        let l ← Tree.arbitraryGo (n / 2)
        let r ← Tree.arbitraryGo (n / 2)
        return Tree.node l r)
    ]

instance : Arbitrary Tree where
  arbitrary := Gen.sized Tree.arbitraryGo

def Tree.shrink : Tree → List Tree
  | .leaf v => (Shrinkable.shrink v).map Tree.leaf
  | .node l r => [l, r] ++ (Tree.shrink l).map (Tree.node · r) ++ (Tree.shrink r).map (Tree.node l ·)

instance : Shrinkable Tree where
  shrink := Tree.shrink

/-- error: None of the deriving handlers for class `ToBinary` applied to `ProofField` -/
#guard_msgs in
structure ProofField where
  val : Nat
  pos : val > 0
deriving ToBinary

/-- error: None of the deriving handlers for class `FromBinary` applied to `ProofField2` -/
#guard_msgs in
structure ProofField2 where
  val : Nat
  pos : val > 0
deriving FromBinary

inductive NoConstructors
deriving ToBinary, FromBinary

/-- Serializes then deserializes `x`, checking the result matches. -/
def roundTrips [ToBinary α] [FromBinary α] [BEq α] (x : α) : Bool :=
  match fromBinary (toBinary x) with
  | .ok y => x == y
  | .error _ => false

/-- `ToBinary`/`FromBinary` deriving round-trips single-ctor, multi-ctor, parametric, and recursive types. -/
def testBlobDeriving : TestM Unit :=
  withHeader "=== Testing Blob ToBinary/FromBinary deriving ===" <| guardTest do
    let checks : List Bool := [
      roundTrips (Pair.mk 42 "hello"),
      roundTrips Color.r, roundTrips Color.g, roundTrips Color.b,
      roundTrips (Shape.circle 5), roundTrips (Shape.rect 3 4),
      roundTrips (Msg.flagged true "hi"), roundTrips (Msg.plain "hi"),
      roundTrips (Cmd.exec (some 3) "go"), roundTrips (Cmd.exec none "go"), roundTrips Cmd.noop,
      roundTrips (Box.mk (5 : Nat)), roundTrips (Box.mk "hi"),
      roundTrips (Tree.node (.leaf 1) (.node (.leaf 2) (.leaf 3)))
    ]
    if !checks.all id then
      throw <| IO.userError s!"expected every Blob deriving round trip to succeed, got {checks}"

    match fromBinaryOf NoConstructors .empty with
    | .error msg =>
      if msg != "Cannot deserialize uninhabited type `NoConstructors`" then
        throw <| IO.userError s!"unexpected error message for uninhabited type: {msg}"
    | .ok _ => throw <| IO.userError "expected deserializing an uninhabited type to fail"

    recordSuccess "Blob ToBinary/FromBinary deriving OK"

/--
Checks `roundTrips` over a wide, automatically-generated sample of `α`, shrinking any
counter-example down to a minimal failing case before reporting it.
-/
def checkRoundTripProperty (α : Type) [ToBinary α] [FromBinary α] [BEq α] [Repr α]
    [Arbitrary α] [Shrinkable α] (label : String) : TestM Unit := do
  -- `maxSize := 500` (vs. Plausible's default 100) so generated `Nat`s reliably exceed 128 and
  -- exercise `ToBinary Nat`'s multi-byte varint continuation-bit path, not just its single-byte one.
  match ← Testable.checkIO (NamedBinder "x" (∀ x : α, roundTrips x)) { maxSize := 500 } with
  | .success _ => recordSuccess s!"{label}: Blob round trip held across generated examples"
  | .gaveUp n => throw <| IO.userError s!"{label}: gave up after {n} attempts satisfying preconditions"
  | .failure _ xs n =>
    throw <| IO.userError (Testable.formatFailure s!"{label}: found a counter-example" xs n)

/--
Property-based counterpart to `testBlobDeriving`: runs `Testable.checkIO` over each fixture type
instead of the hand-picked examples above, sampling a wide, automatically-generated value space
(large `Nat`s, empty/long/Unicode strings, deeply nested `Tree`s) with automatic shrinking of any
counter-example found.
-/
def testBlobDerivingProperties : TestM Unit :=
  withHeader "=== Testing Blob ToBinary/FromBinary deriving (property-based) ===" <| guardTest do
    checkRoundTripProperty Pair "Pair"
    checkRoundTripProperty Color "Color"
    checkRoundTripProperty Shape "Shape"
    checkRoundTripProperty Msg "Msg"
    checkRoundTripProperty Cmd "Cmd"
    checkRoundTripProperty (Box Nat) "Box Nat"
    checkRoundTripProperty (Box String) "Box String"
    checkRoundTripProperty Tree "Tree"
