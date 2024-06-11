module CompExpr.ExprHelpers

open System.Diagnostics
open System.IO
open Fantomas
open FSharp.Compiler
open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia
open FSharp.Compiler.Xml
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Text
open System.Reflection
open System

let tmpRange = Text.range ()

type Ident with

    static member ofString(s) = Ident(s, Text.range ())

type SynExpr with

    member expr.WrapInParens() =
        SynExpr.Paren(expr, range.Zero, Some range.Zero, range.Zero)

    member expr.BodyOfLambda(arg) = expr.BodyOfLambda([ arg ])

    /// f expr
    member args.ApplyTo(f) =
        SynExpr.App(ExprAtomicFlag.Atomic, false, funcExpr = f, argExpr = args, range = Text.range ())

    member args.ApplyToInfix(f) =
        SynExpr.App(ExprAtomicFlag.Atomic, true, funcExpr = f, argExpr = args, range = Text.range ())

    member expr.BodyOfLambda(args) =
        SynExpr.Lambda(
            fromMethod = false,
            inLambdaSeq = false,
            args = SynSimplePats.SimplePats([], expr.Range), //dunno about this
            //arrow = Some (range.Zero),
            body = expr,
            parsedData = Some(args, expr),
            range = expr.Range,
            trivia = { ArrowRange = Some(range.Zero) }
        )

    member expr.WithTypeArgs(typeArgs) =
        SynExpr.TypeApp(
            expr = expr, 
            lessRange = Text.range (),

            typeArgs = typeArgs, 
            commaRanges = [],
            greaterRange = Some(Text.range ()),
            typeArgsRange = Text.range (),
            range = Text.range ()
        )

    member expr.RequireParens() =
        match expr with
        | SynExpr.Lambda _ ->
            true
        | _ -> false


let makeType (str: string) =
    let typeName =
        str |> (fun s -> s.Split(".")) |> Array.map Ident.ofString |> List.ofArray
    SynType.LongIdent(LongIdentWithDots(typeName, []))

let makeGenericType (typeName) (genericTypes) =
    SynType.App(typeName, Some tmpRange, genericTypes, [], Some tmpRange, false, tmpRange)

//let makeGenericType' (typeName) (genericTypes) =
//    SynType.LongIdentApp (typeName, Some tmpRange, genericTypes, [], Some tmpRange, false,  tmpRange)

let makeGenericArguments (fsTypes: FSharpType list) =
    fsTypes
    |> List.map (fun fsType ->
        if fsType.IsGenericParameter then
            SynType.Var(SynTypar(Ident.ofString fsType.GenericParameter.Name, TyparStaticReq.None, false), tmpRange)
        else
            makeType fsType.TypeDefinition.DisplayName)

let toSynTypeFromClrType (name: string) (fsTypes: FSharpType list) =
    if fsTypes.Length > 0 then
        let inner = makeGenericArguments fsTypes
        makeGenericType (makeType name) inner
    else
        makeType name

let rec toSynType (fsType: FSharpType) =
    if fsType.IsGenericParameter then
        //makeType ("'" + fsType.GenericParameter.FullName)
        SynType.Var(SynTypar(Ident.ofString fsType.GenericParameter.FullName, TyparStaticReq.None, false), tmpRange)
    elif fsType.GenericArguments.Count > 0 then
        let inner = fsType.GenericArguments |> Seq.map toSynType |> Seq.toList

        makeGenericType
            (makeType fsType.TypeDefinition.DisplayName) //todo namespace
            inner

    else
        makeType fsType.TypeDefinition.DisplayName

let createTyped (paramName: string) t =
    SynPat.Paren(
        SynPat.Typed(SynPat.Named(Ident.ofString (paramName.Replace("@", "")), false, None, tmpRange), t, tmpRange),
        tmpRange
    )

let getSynType (fullType: FSharpType) =
    if (fullType.HasTypeDefinition) then
        toSynType fullType
    elif (fullType.IsTupleType) then
        //todo
        SynType.Tuple(
            fullType.IsStructTupleType,
            [ for _ in fullType.GenericArguments -> false, SynType.Anon(tmpRange) ],
            tmpRange
        )
    else
        let typeName =
            fullType.AbbreviatedType.TypeDefinition.FullName
            |> fun s -> s.Split(".")
            |> Array.map Ident.ofString
            |> List.ofArray

        SynType.LongIdent(LongIdentWithDots(typeName, [ Text.range.Zero ]))

let createSynPat (fullType: FSharpType) (logicalName) =
    if (fullType.HasTypeDefinition) then
        let synType = getSynType fullType
        createTyped logicalName synType

    elif (fullType.IsTupleType) then
        //todo
        let tupleType = getSynType fullType

        SynPat.Paren(
            SynPat.Typed(
                SynPat.Named(Ident.ofString (logicalName.Replace("@", "")), false, None, tmpRange),
                tupleType,
                tmpRange
            ),
            tmpRange
        )
    else
        SynPat.Paren(SynPat.Named(Ident.ofString (logicalName.Replace("@", "")), false, None, tmpRange), tmpRange)

let getArgs (memberOrFunctionValue: FSharpMemberOrFunctionOrValue) =
    if memberOrFunctionValue.FullName.StartsWith "unitVar" then
        SynPat.Paren(SynPat.Const(SynConst.Unit, tmpRange), tmpRange)
    else
        createSynPat memberOrFunctionValue.FullType memberOrFunctionValue.LogicalName

let createBinding (value: FSharpMemberOrFunctionOrValue) body =
    let headPat = SynPat.Named(Ident.ofString value.LogicalName, value.IsMemberThisValue, None, Text.range ())
    SynBinding(
        None,
        SynBindingKind.Normal,//standAlone?
        false,
        value.IsMutable,
        [],
        PreXmlDoc.Empty,
        valData = SynValData(None, SynValInfo([], SynArgInfo([], false, None)), None),
        headPat = headPat,
        //SynPat.Wild(Text.range.Zero),
        returnInfo = Some(SynBindingReturnInfo(SynType.Anon(range.Zero), range.Zero, [])),
        expr = body,
        range = range.Zero,
        debugPoint = DebugPointAtBinding.Yes(Text.range ()),
        trivia = {
            LetKeyword = Some(range.Zero)
            EqualsRange = Some(range.Zero)
        }
    )

let createLetBinding from to_ =
    SynBinding(
        None,
        SynBindingKind.Normal,//standAlone?
        false,
        false,
        [],
        PreXmlDoc.Empty,
        valData = SynValData(None, SynValInfo([], SynArgInfo([], false, None)), None),
        headPat = SynPat.Named(Ident.ofString to_, false, None, Text.range ()),
        //SynPat.Wild(Text.range.Zero),
        returnInfo = Some(SynBindingReturnInfo(SynType.Anon(range.Zero), range.Zero, [])),
        expr = SynExpr.Ident(Ident(from, range.Zero)),
        range = range.Zero,
        debugPoint = DebugPointAtBinding.Yes(Text.range ()),
        trivia = {
            LetKeyword = Some(range.Zero)
            EqualsRange = Some(range.Zero)
        }
    )


let createIdent (args) =
    let args =
        args
        |> List.map (fun (s: string) -> s.Replace("@", ""))
        |> List.map (Ident.ofString)

    SynExpr.LongIdent(false, LongIdentWithDots(args, [ Text.range.Zero ]), None, Text.range.Zero)

let createTupled a =
    SynExpr.Tuple(false, a, [], Text.range.Zero)
