module rec CompExpr.MapperV2

open FSharp.Compiler
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Symbols.FSharpExprPatterns
open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia
open FSharp.Compiler.Text
open System.Runtime.CompilerServices

open System
open FSharp.Compiler.Xml

type String with
    member this.Named() =
        SynPat.Named(this.Replace("@", "").Ident(), false, None, Range.Zero)

    member this.Typed(typ: SynType) =
        this.Named().TypedPat(typ)

    member this.Typed(typ: FSharpType) =
        this.Named().TypedPat(typ.toSynType())

    member this.Ident() = Ident(this, Range.Zero)

    member this.LongIdent() =
        let typeName =
            this.Split(".") |> Array.map (_.Ident()) |> List.ofArray
        SynType.LongIdent(LongIdentWithDots(typeName, []))

    member this.Var() =
        SynType.Var(SynTypar(this.Ident(), TyparStaticReq.None, false), Range.Zero)

    member this.Const() =
        SynExpr.Const(SynConst.String(this, SynStringKind.Regular, Range.Zero), Range.Zero)

    member this.IdentExpr() =   
        SynExpr.Ident(this.Ident())

type SynPat with
    member this.TypedPat(typ) = 
        SynPat.Typed(this, typ, Range.Zero)

type SynExpr with

    member expr.WrapInParens() =
        SynExpr.Paren(expr, range.Zero, Some range.Zero, range.Zero)

    member expr.LambdaExpr(arg) = expr.LambdaExpr([ arg ])

    member this.Apply(args) =
        SynExpr.App(
            flag = ExprAtomicFlag.NonAtomic,
            isInfix = false,
            funcExpr = this,
            argExpr = args,
            range = range.Zero
        )

    member this.Apply(args: string) =
        this.Apply(args.Const())

    /// f expr
    member args.ApplyTo(f) =
        SynExpr.App(ExprAtomicFlag.Atomic, false, funcExpr = f, argExpr = args, range = Range.Zero)

    member args.ApplyToInfix(f) =
        SynExpr.App(ExprAtomicFlag.Atomic, true, funcExpr = f, argExpr = args, range = Range.Zero)

    member expr.WithTypeArgs(typeArgs) =
        SynExpr.TypeApp(
            expr = expr, 
            lessRange = Range.Zero,
            typeArgs = typeArgs, 
            commaRanges = [],
            greaterRange = Some(Range.Zero),
            typeArgsRange = Range.Zero,
            range = Range.Zero
        )

    member expr.RequireParens() =
        match expr with
        | SynExpr.Lambda _ ->
            true
        | _ -> false

type String with
    member this.Apply(args: string) =
        this.IdentExpr().Apply(args)

    member this.Apply(args: SynExpr) =
        this.IdentExpr().Apply(args)

[<ExtensionAttribute>]
type ListExtensions =
    [<Extension>]
    static member LongIdentWithDots(args: string list) =
        LongIdentWithDots(args |> List.map (_.Replace("@", "").Ident()),  [ Range.Zero ])

    [<Extension>]
    static member LongIdent(this: string list) =
        SynExpr.LongIdent(false, this.LongIdentWithDots(), None, Range.Zero)

    [<Extension>]
    static member Tuple(this: SynExpr list) =
        SynExpr.Tuple(false, this, [], Range.Zero)

    [<Extension>]
    static member LambdaExpr(this: SynExpr, args: SynPat list) =
        SynExpr.Lambda(
            fromMethod = false,
            inLambdaSeq = false,
            args = SynSimplePats.SimplePats([], Range.Zero), //dunno about this
            //arrow = Some (range.Zero),
            body = this,
            parsedData = Some(args, this),
            range = Range.Zero,
            trivia = { ArrowRange = Some(range.Zero) }
        )

    [<Extension>]
    static member LambdaExpr(this: SynExpr, arg: SynPat) =
        this.LambdaExpr([ arg ])

    [<Extension>]
    static member Tuple(this: FSharpExpr list) =
        this |> List.map (_.ToUntyped()) |> _.Tuple()

type SynType with 
    member this.Args(genericTypes) =
        SynType.App(this, Some(Range.Zero), genericTypes, [], Some(Range.Zero), false, Range.Zero)

type FSharpType with
    member fullType.Tuple() =
        SynType.Tuple(
            fullType.IsStructTupleType,
            [ for _ in fullType.GenericArguments -> false, SynType.Anon(Range.Zero ) ],
            Range.Zero 
        )

    member fsType.toSynType () =
        if (fsType.HasTypeDefinition) then
            if fsType.IsGenericParameter then
                fsType.GenericParameter.FullName.Var()
            elif fsType.GenericArguments.Count > 0 then
                let inner = fsType.GenericArguments |> Seq.map (_.toSynType()) |> Seq.toList
                fsType.TypeDefinition.DisplayName.LongIdent().Args(inner)
            else
                fsType.TypeDefinition.DisplayName.LongIdent()

        elif (fsType.IsTupleType) then
            //todo
            fsType.Tuple()
        else
            fsType.AbbreviatedType.TypeDefinition.FullName.LongIdent()

    member fullType.createSynPat (logicalName: string) =
        if (fullType.HasTypeDefinition) then
            logicalName.Typed(fullType)

        elif (fullType.IsTupleType) then
            //todo
            SynPat.Paren(
                (logicalName.Typed(fullType)),
                Range.Zero
            )
        else
            SynPat.Paren(logicalName.Named(), Range.Zero)

type FSharpMemberOrFunctionOrValue with
    member this.Named() =
        SynPat.Named(this.LogicalName.Ident(), this.IsMemberThisValue, None, Range.Zero)

    member this.getArgs () =
        if this.FullName.StartsWith "unitVar" then
            SynPat.Paren(SynPat.Const(SynConst.Unit, Range.Zero), Range.Zero)
        else
            this.FullType.createSynPat this.LogicalName
            
    member value.createBinding body =
        SynBinding(
            None,
            SynBindingKind.Normal,
            false,
            value.IsMutable,
            [],
            PreXmlDoc.Empty,
            valData = SynValData(None, SynValInfo([], SynArgInfo([], false, None)), None),
            headPat = value.Named(),
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



type FSharpExpr with
    member this.ToUntyped(): SynExpr =
        match this with
        | Application(expr, types, [ arg ]) ->
            expr.ToUntyped().WrapInParens().Apply(arg.ToUntyped())

        | Application(expr, types, args) ->
            expr.ToUntyped().WrapInParens().Apply(args.Tuple())
        | Lambda(args, expr) ->
            expr.ToUntyped().LambdaExpr(args.getArgs())
        | Call c ->
            failwith ""
        | a ->
            "invalidArg".Apply("fsharpExpr").Apply(sprintf "%A." a)