/-
Copyright (c) 2026 Paul Butcher. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module

public import Postgres
public import Tests.ModularDeriving

public section

open Postgres

def readPerson : RowReader Person := Row.read

def readCoordinate : RowReader Coordinate := Row.read

def getUserId (stmt : Stmt) (column : Int32) : IO UserId := ResultColumn.get stmt column

def bindUserId (stmt : Stmt) (index : Int32) (u : UserId) : IO Unit :=
  QueryParam.bind stmt index u

def bindPort (stmt : Stmt) (index : Int32) (p : Port) : IO Unit :=
  QueryParam.bind stmt index p
