/-
Copyright (c) 2026 Paul Butcher. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module

public import Postgres

/-!
The deriving handlers have to work from a `module`, which is the only configuration in which a
consumer sheds the Lean frontend. The aux definitions they emit are therefore visible-but-unexposed
rather than `private`, whose mangled name a `module` cannot refer back to. `Tests.ModularDeriving`
covers elaborating the `deriving` commands; `Tests.ModularDerivingUse` covers reaching the
resulting instances from a downstream module.
-/

public section

open Postgres

structure Person where
  name : String
  age : Int32
deriving Row

structure Coordinate where
  x : Float
  y : Option Float

structure UserId where
  value : Int64
deriving ResultColumn, QueryParam

structure Port where
  value : Int32
  inRange : value > 0

deriving instance Row for Coordinate
deriving instance QueryParam for Port
