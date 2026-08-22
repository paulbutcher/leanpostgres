/-
Copyright (c) 2026 Paul Butcher. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module

public meta import Lean.Elab.Deriving.Basic
public meta import Lean.Elab.Deriving.Util
import Postgres.Blob.Classes

namespace Postgres.Blob

set_option doc.verso true
set_option linter.missingDocs true

open Lean Elab Meta Parser Term Command
open Elab.Deriving

/-!
Neither generated function is {lit}`partial`. A serializer recurses on the value, which is
structural. A deserializer recurses on the data, where nothing tells Lean that a field is smaller
than the value it came out of, so a recursive type's deserializer takes a count instead, which the
instance seeds from the bytes still unread. That count is never what stops it: every level reads at
least one byte, so it cannot run out before the data does. A type that cannot recurse carries no
count.
-/

/-! # Helpers -/

/--
Gets the {lean}`InductiveVal` for a name, if it exists.
-/
private meta def getInductiveVal? (env : Environment) (name : Name) : Option InductiveVal :=
  match env.find? name with
  | some (.inductInfo val) => some val
  | _ => none

/--
Variant of {name}`Lean.Elab.Deriving.mkHeader` that doesn't add an explicit binder for the target
value. We only need implicit type parameters and instance binders.
-/
private meta def mkHeader (constraintClass : Name) (indVal : InductiveVal) : TermElabM Header := do
  let argNames ← mkInductArgNames indVal
  let binders ← mkImplicitBinders argNames
  let targetType ← mkInductiveApp indVal argNames
  let instBinders ← mkInstImplicitBinders constraintClass indVal argNames
  let binders : Array (TSyntax `Lean.Parser.Term.bracketedBinder) := (binders ++ instBinders).map (⟨·⟩)
  return {
    binders := binders
    argNames := argNames
    targetNames := #[]
    targetType := targetType
  }

/--
Checks whether any constructor field's type depends on a previous field. Returns {name}`true` if
there are no dependencies.
-/
private meta def hasNoFieldDependencies (ctorName : Name) : MetaM Bool := do
  let ctorInfo ← getConstInfoCtor ctorName
  forallTelescopeReducing ctorInfo.type fun args _ => do
    let fieldArgs := args[ctorInfo.numParams:].toArray
    let mut prevFVars : Std.HashSet FVarId := {}
    for i in [:fieldArgs.size] do
      let argType ← inferType fieldArgs[i]!
      if argType.hasAnyFVar (prevFVars.contains ·) then
        return false
      prevFVars := prevFVars.insert fieldArgs[i]!.fvarId!
    return true

/--
Checks whether all constructor fields are non-proof (data) fields.
Returns {name}`false` if any field has a {lean}`Prop` type.
-/
private meta def hasNoProofFields (ctorName : Name) : MetaM Bool := do
  let ctorInfo ← getConstInfoCtor ctorName
  forallTelescopeReducing ctorInfo.type fun args _ => do
    let fieldArgs := args[ctorInfo.numParams:].toArray
    for i in [:fieldArgs.size] do
      let argType ← inferType fieldArgs[i]!
      if ← isProp argType then
        return false
    return true

/--
Returns the tag type name: {name}`UInt8` if ≤ 256 constructors, otherwise {name}`Nat`.
-/
private meta def tagTypeName (numCtors : Nat) : Name :=
  if numCtors ≤ 256 then ``UInt8 else ``Nat

/--
Runs {name}`k` with the types of a constructor's explicit fields, under the local context that
binds them.
-/
private meta def withFieldTypes (ctorName : Name) (k : Array Expr → TermElabM α) : TermElabM α := do
  let ctorInfo ← getConstInfoCtor ctorName
  forallTelescopeReducing ctorInfo.type fun args _ => do
    k (← args[ctorInfo.numParams:].toArray.mapM fun arg => inferType arg)

/--
Whether a type mentions the inductive being derived.
-/
private meta def mentions (indName : Name) (type : Expr) : Bool :=
  (type.find? (·.isConstOf indName)).isSome

/--
The element type, for a one-argument application of {name}`container`.
-/
private meta def containerArg? (container : Name) (type : Expr) : Option Expr :=
  if type.isAppOfArity container 1 then type.getAppArgs[0]? else none

/--
Reports a field whose type mentions the one being derived in a shape with no code to generate.
-/
private meta def unsupportedField (indName : Name) (type : Expr) : TermElabM α :=
  throwError "cannot derive a binary codec for {indName}: no rule covers a field of type {type}\n\
Recursion is generated through a field of type {indName} itself, or of `Array`, `List` or \
`Option` of it; anything else has to be written by hand."

/-! # ToBinary Generation -/

/--
The step that appends one field to the bytes accumulated so far: the function being defined where
the field recurses, the {name}`ToBinary` instance otherwise.
-/
private meta def serializeField (indName : Name) (aux : Ident) (x : Ident) (type : Expr) :
    TermElabM Term := do
  if type.isAppOf indName then
    `($aux $x)
  else if let some inner := containerArg? ``Array type then
    if inner.isAppOf indName then
      -- The mapped array is bound rather than folded over in place: `Array.foldl` defaults its
      -- `stop` to `as.size`, which would repeat the mapped array, and a second occurrence of the
      -- recursive call is more than the termination checker can eliminate.
      `(fun b => let parts := ($x).map $aux;
                 parts.foldl (fun s g => g s) (ToBinary.serializer ($x).size b))
    else viaInstance type
  else if let some inner := containerArg? ``List type then
    if inner.isAppOf indName then
      `(fun b => let parts := ($x).map $aux;
                 parts.foldl (fun s g => g s) (ToBinary.serializer ($x).length b))
    else viaInstance type
  else if let some inner := containerArg? ``Option type then
    if inner.isAppOf indName then
      -- A `match` rather than `Option.map`: a recursive call passed as an argument defeats the
      -- structural recursion this definition relies on.
      `(fun b => match $x:term with | none => b.push 0 | some v => $aux v (b.push 1))
    else viaInstance type
  else
    viaInstance type
where
  viaInstance (type : Expr) : TermElabM Term := do
    if mentions indName type then unsupportedField indName type else `(ToBinary.serializer $x)

/--
Generates the {name}`ToBinary` body for a zero-constructor type.
-/
private meta def mkToBinaryZeroCtorBody : TermElabM Term := `(nofun)

/--
Generates the {name}`ToBinary` body for a single-constructor type. No tag is emitted; fields are serialized
sequentially.
-/
private meta def mkToBinarySingleCtorBody (indVal : InductiveVal) (aux : Ident) : TermElabM Term :=
  withFieldTypes indVal.ctors[0]! fun types => do
    let fieldNames : Array Name := (Array.range types.size).map fun i => Name.mkSimple s!"f_{i}"
    let patternElems : Array (TSyntax `term) := fieldNames.map fun n => mkIdent n
    let pattern ← `(⟨$patternElems,*⟩)
    let mut result : TSyntax `term ← `(acc)
    for i in [:types.size] do
      let step ← serializeField indVal.name aux (mkIdent fieldNames[i]!) types[i]!
      result ← `($result |> $step)
    `(fun | $pattern, acc => $result)

/--
Generates the ToBinary body for a multi-constructor type. Each constructor gets a sequential tag
({name}`UInt8` or {name}`Nat`), then fields are serialized.
-/
private meta def mkToBinaryMultiCtorBody (indVal : InductiveVal) (aux : Ident) : TermElabM Term := do
  let tagType := tagTypeName indVal.ctors.length
  let mut arms : Array (TSyntax ``matchAlt) := #[]
  for ctorIdx in [:indVal.ctors.length] do
    let ctorName := indVal.ctors[ctorIdx]!
    let arm ← withFieldTypes ctorName fun types => do
      let fieldNames : Array Name := (Array.range types.size).map fun i => Name.mkSimple s!"f_{i}"
      let patternElems : Array (TSyntax `term) := fieldNames.map fun n => mkIdent n
      let pattern ← `($(mkCIdent ctorName) $patternElems*)
      let tagLit ← `(($(Syntax.mkNumLit (toString ctorIdx)) : $(mkIdent tagType)))
      let mut result ← `(acc |> ToBinary.serializer $tagLit)
      for i in [:types.size] do
        let step ← serializeField indVal.name aux (mkIdent fieldNames[i]!) types[i]!
        result ← `($result |> $step)
      `(matchAltExpr| | $pattern, acc => $result)
    arms := arms.push arm
  `(fun $arms:matchAlt*)

/--
Generates the auxiliary function definition for {name}`ToBinary`.
-/
private meta def mkToBinaryAuxFunction (ctx : Deriving.Context) (i : Nat) : TermElabM Command := do
  let aux := Lean.mkIdent ctx.auxFunNames[i]!
  let indVal := ctx.typeInfos[i]!
  let header ← mkHeader ``ToBinary indVal
  let targetType := header.targetType

  let body ← match indVal.ctors.length with
    | 0 => mkToBinaryZeroCtorBody
    | 1 => mkToBinarySingleCtorBody indVal aux
    | _ => mkToBinaryMultiCtorBody indVal aux

  `(@[no_expose] def $aux $header.binders:bracketedBinder* : Serializer $targetType := $body)

/--
Creates instance commands for {name}`ToBinary`.
-/
private meta def mkToBinaryInstanceCmds (ctx : Deriving.Context) (typeNames : Array Name) : TermElabM (Array Command) := do
  let mut instances := #[]
  for i in [:ctx.typeInfos.size] do
    let indVal := ctx.typeInfos[i]!
    if typeNames.contains indVal.name then
      let auxFunName := ctx.auxFunNames[i]!
      let argNames ← mkInductArgNames indVal
      let binders ← mkImplicitBinders argNames
      let binders := binders ++ (← mkInstImplicitBinders ``ToBinary indVal argNames)
      let binders : TSyntaxArray `Lean.Parser.Term.implicitBinder := binders.map (⟨·⟩)
      let indType ← mkInductiveApp indVal argNames
      let type ← `(ToBinary $indType)
      let val ← `(⟨$(Lean.mkIdent auxFunName)⟩)
      let instCmd ← `(instance $binders:implicitBinder* : $type := $val)
      instances := instances.push instCmd
  return instances

/--
The main deriving handler for {name}`ToBinary`.
-/
meta def mkToBinaryInstanceHandler (declNames : Array Name) : CommandElabM Bool := do
  let env ← getEnv
  if ← declNames.allM fun name => do
    let some indVal := getInductiveVal? env name | return false
    liftTermElabM do
      for ctorName in indVal.ctors do
        if !(← hasNoProofFields ctorName) then return false
        if !(← hasNoFieldDependencies ctorName) then return false
      return true
  then
    let some firstDecl := declNames[0]? | return false
    let ctx ← liftTermElabM <| mkContext ``ToBinary "toBinaryAux" firstDecl
    let auxFunCmd ← liftTermElabM <| mkToBinaryAuxFunction ctx 0
    elabCommand auxFunCmd
    let instanceCmds ← liftTermElabM <| mkToBinaryInstanceCmds ctx declNames
    instanceCmds.forM elabCommand
    return true
  else
    return false

/--
Wraps a constructor in an explicit lambda: generates `fun f_0 f_1 ... => Ctor f_0 f_1 ...`.
-/
private meta def mkCtorLambda (ctorName : Name) (numFields : Nat) : TermElabM Term := do
  let fieldNames : Array (TSyntax `ident) := (Array.range numFields).map fun i => mkIdent (Name.mkSimple s!"f_{i}")
  let ctorApp ← `($(mkCIdent ctorName) $fieldNames*)
  `(fun $fieldNames* => $ctorApp)

/-! # FromBinary Generation -/

/--
The deserializer for one field, passing whatever count the enclosing call has left down through
any recursion.
-/
private meta def deserializeField (indName : Name) (aux : Ident) (count : Ident) (type : Expr) :
    TermElabM Term := do
  if type.isAppOf indName then
    `($aux $count)
  else if let some inner := containerArg? ``Array type then
    if inner.isAppOf indName then `(Deserializer.arrayOf ($aux $count)) else viaInstance type
  else if let some inner := containerArg? ``List type then
    if inner.isAppOf indName then `(Deserializer.listOf ($aux $count)) else viaInstance type
  else if let some inner := containerArg? ``Option type then
    if inner.isAppOf indName then `(Deserializer.optionOf ($aux $count)) else viaInstance type
  else
    viaInstance type
where
  viaInstance (type : Expr) : TermElabM Term := do
    if mentions indName type then unsupportedField indName type else `(FromBinary.deserializer)

/--
Reads a constructor's fields in order and applies the constructor to them.
-/
private meta def mkCtorRead (indName : Name) (aux : Ident) (count : Ident) (ctorName : Name)
    (types : Array Expr) : TermElabM Term := do
  if types.size == 0 then
    `(pure ($(mkCIdent ctorName) : _))
  else
    let ctorFn ← mkCtorLambda ctorName types.size
    let mut result ← `($ctorFn <$> $(← deserializeField indName aux count types[0]!))
    for i in [1:types.size] do
      result ← `($result <*> $(← deserializeField indName aux count types[i]!))
    return result

/--
Generates the {name}`FromBinary` body for a zero-constructor (uninhabited) type. The generated
deserializer immediately throws an error.
-/
private meta def mkFromBinaryZeroCtorBody (indVal : InductiveVal) : TermElabM Term := do
  let errorMsg := s!"Cannot deserialize uninhabited type `{indVal.name}`"
  let errorMsgLit := Syntax.mkStrLit errorMsg
  `(throw $errorMsgLit)

/--
Generates the {name}`FromBinary` body for a single-constructor type.
-/
private meta def mkFromBinarySingleCtorBody (indVal : InductiveVal) (aux : Ident) (count : Ident) :
    TermElabM Term :=
  withFieldTypes indVal.ctors[0]! fun types =>
    if types.size == 0 then `(pure ⟨⟩)
    else mkCtorRead indVal.name aux count indVal.ctors[0]! types

/--
Generates the {name}`FromBinary` body for a multi-constructor type. Reads a tag, then dispatches to
the appropriate constructor.
-/
private meta def mkFromBinaryMultiCtorBody (indVal : InductiveVal) (aux : Ident) (count : Ident) :
    TermElabM Term := do
  let numCtors := indVal.ctors.length
  let tagType := tagTypeName numCtors

  let mut matchArms : Array (TSyntax ``matchAlt) := #[]
  for ctorIdx in [:numCtors] do
    let ctorName := indVal.ctors[ctorIdx]!
    let tagLit := Syntax.mkNumLit (toString ctorIdx)
    let armBody ← withFieldTypes ctorName (mkCtorRead indVal.name aux count ctorName)
    let matchArm ← `(matchAltExpr| | $tagLit => $armBody)
    matchArms := matchArms.push matchArm

  -- Error arm
  let typeName := indVal.name
  let errorMsg := s!"Expected tag 0-{numCtors - 1} for `{typeName}`, got "
  let errorMsgLit := Syntax.mkStrLit errorMsg
  let errorArm ← `(matchAltExpr| | other => throw ($errorMsgLit ++ toString other))
  matchArms := matchArms.push errorArm

  `(FromBinary.deserializer >>= fun (tag : $(mkIdent tagType)) =>
      match tag with $matchArms:matchAlt*)

/--
Generates the auxiliary function definition for FromBinary.
-/
private meta def mkFromBinaryAuxFunction (ctx : Deriving.Context) (i : Nat) : TermElabM Command := do
  let aux := Lean.mkIdent ctx.auxFunNames[i]!
  let indVal := ctx.typeInfos[i]!
  let header ← mkHeader ``FromBinary indVal
  let targetType := header.targetType
  let count := mkIdent (← mkFreshUserName `count)

  let body ← match indVal.ctors.length with
    | 0 => mkFromBinaryZeroCtorBody indVal
    | 1 => mkFromBinarySingleCtorBody indVal aux count
    | _ => mkFromBinaryMultiCtorBody indVal aux count

  if indVal.isRec then
    let exhausted := Syntax.mkStrLit s!"`{indVal.name}` nested deeper than the data can represent"
    `(@[no_expose] def $aux $header.binders:bracketedBinder* ($count : Nat) : Deserializer $targetType :=
        match $count:ident with
        | 0 => throw $exhausted
        | $count:ident + 1 => $body)
  else
    `(@[no_expose] def $aux $header.binders:bracketedBinder* : Deserializer $targetType := $body)

/--
Creates instance commands for {name}`FromBinary`.
-/
private meta def mkFromBinaryInstanceCmds (ctx : Deriving.Context) (typeNames : Array Name) : TermElabM (Array Command) := do
  let mut instances := #[]
  for i in [:ctx.typeInfos.size] do
    let indVal := ctx.typeInfos[i]!
    if typeNames.contains indVal.name then
      let aux := Lean.mkIdent ctx.auxFunNames[i]!
      let argNames ← mkInductArgNames indVal
      let binders ← mkImplicitBinders argNames
      let binders := binders ++ (← mkInstImplicitBinders ``FromBinary indVal argNames)
      let binders : TSyntaxArray `Lean.Parser.Term.implicitBinder := binders.map (⟨·⟩)
      let indType ← mkInductiveApp indVal argNames
      let type ← `(FromBinary $indType)
      -- The unread bytes bound how deep the data can go, so they are count enough for anything a
      -- serializer produced.
      let val ← if indVal.isRec then `(⟨fun s => $aux (s.data.size - s.cursor) s⟩) else `(⟨$aux⟩)
      let instCmd ← `(instance $binders:implicitBinder* : $type := $val)
      instances := instances.push instCmd
  return instances

/--
The main deriving handler for {name}`FromBinary`.
-/
meta def mkFromBinaryInstanceHandler (declNames : Array Name) : CommandElabM Bool := do
  let env ← getEnv
  if ← declNames.allM fun name => do
    let some indVal := getInductiveVal? env name | return false
    liftTermElabM do
      for ctorName in indVal.ctors do
        if !(← hasNoProofFields ctorName) then return false
        if !(← hasNoFieldDependencies ctorName) then return false
      return true
  then
    let some firstDecl := declNames[0]? | return false
    let ctx ← liftTermElabM <| mkContext ``FromBinary "fromBinaryAux" firstDecl
    let auxFunCmd ← liftTermElabM <| mkFromBinaryAuxFunction ctx 0
    elabCommand auxFunCmd
    let instanceCmds ← liftTermElabM <| mkFromBinaryInstanceCmds ctx declNames
    instanceCmds.forM elabCommand
    return true
  else
    return false

/-! # Registration -/

meta initialize
  registerDerivingHandler ``ToBinary mkToBinaryInstanceHandler

meta initialize
  registerDerivingHandler ``FromBinary mkFromBinaryInstanceHandler

end Postgres.Blob
