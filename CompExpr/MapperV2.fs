module rec CompExpr.MapperV2

open FSharp.Compiler
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Symbols.FSharpExprPatterns

open Fantomas.FCS.Syntax
open Fantomas.FCS.SyntaxTrivia
open Fantomas.FCS.Text

open System.Runtime.CompilerServices

open System
open Fantomas.FCS.Xml
open System.Collections.Generic


let (|Tree|_|) fsharpExpr =
    match fsharpExpr with
    | DecisionTree(MatchCase(guard, case2, case3), nodes) ->               
        match case2, case3 with
        | (Jump nodes (names1, case1)), (Jump nodes (names2, case2)) ->
            Some(guard, names1, case1, names2, case2)
        | _ -> None
    | _ -> None

let (|Jump|_|) (nodes: (FSharpMemberOrFunctionOrValue list * FSharpExpr) list) (fsharpExpr: FSharpExpr) =
    match fsharpExpr with
    | DecisionTreeSuccess(i, _) -> Some(nodes[i])
    | _ -> None

let (|LetBound|_|) (fsharpExpr: FSharpExpr) =
    let rec letBound (fsharpExpr: FSharpExpr) acc =
        match fsharpExpr with
        | Call _
        | Const _
        | Value _ -> Some(acc, fsharpExpr)
        | Let((f, UnionCaseGet(_), _), node) -> letBound node (f.CompiledName :: acc)
        | Let((f, TupleGet(_), _), node) -> letBound node (f.CompiledName :: acc)
        | _ -> None

    letBound fsharpExpr []

let (|MatchCase|_|) (fsharpExpr: FSharpExpr) =
    match fsharpExpr with
    | IfThenElse(UnionCaseTest(_, _, unionCase), thenCase, elseCase) -> Some(unionCase, thenCase, elseCase)
    | _ -> None

let (|DirectMatch|_|) (nodes: (FSharpMemberOrFunctionOrValue list * FSharpExpr) list) (fsharpExpr: FSharpExpr) =
    match fsharpExpr with
    | IfThenElse(Call(_) as guard, (Jump nodes (_, n1)), n2) -> Some(guard, n1, n2)
    | _ -> None

let (|WildCardCase|_|) (nodes: (FSharpMemberOrFunctionOrValue list * FSharpExpr) list) (fsharpExpr: FSharpExpr) =
    match fsharpExpr with
    | IfThenElse(Call(_) as guard, (Jump nodes (_, n1)), (Jump nodes (_, n2))) -> Some(guard, n1, n2)
    | _ -> None


let (|MatchUnionCase|_|) (nodes: (FSharpMemberOrFunctionOrValue list * FSharpExpr) list) (fsharpExpr: FSharpExpr) =
    //terminal node directly from a UnionCaseTest
    match fsharpExpr with
    | MatchCase(case, (Const(i, _xz) as node), elseCase) ->
        let matchPattern = case.LongIdent() //mkMatchPattern case []
        //and use these as names in the match.
        //then we can remove unneeded let-bindings.
        let clause = matchPattern.CreateSynMatchClause(node.ToUntyped())

        Some(clause, None, elseCase)

    | MatchCase(case, (Jump nodes (_, LetBound(names, node))), elseCase) ->
        //have the case-name.
        //by resolving the bound node, we can find the names that are in scope
        let matchPattern = case.LongIdent(names)
        //and use these as names in the match.
        //then we can remove unneeded let-bindings.
        let clause = matchPattern.CreateSynMatchClause(node.ToUntyped())
        Some(clause, None, elseCase)

    //the when-case
    | MatchCase(case, IfThenElse(LetBound(_, guard), (Jump nodes (names1, case1)), (Jump nodes (names2, case2))), rest) ->
        //case when guard is ok.
        //if the end-node here is another if-else or union-case-test, not sure we can generate good code.
        //so assume that does not happen.

        let matchPattern = case.LongIdent [ for a in names1 -> a.CompiledName ]
        //and use these as names in the match.
        //then we can remove unneeded let-bindings.
        let matchPattern2 = case.LongIdent [ for a in names2 -> a.CompiledName ]

        let clause1 =
            matchPattern.CreateSynMatchClause(case1.ToUntyped(), guard.ToUntyped())

        let clause2 =
            match case2 with
            //see if we can inline. but this only inlines so far...
            | Tree(guard, _, b, _, _) when guard.CompiledName = case.CompiledName ->
                matchPattern2.CreateSynMatchClause(b.ToUntyped())
            | Tree(_,_,_,_, d) ->
                matchPattern2.CreateSynMatchClause(d.ToUntyped())
            | MatchCase(guard, case2, _) when guard.CompiledName = case.CompiledName ->
                matchPattern2.CreateSynMatchClause(case2.ToUntyped())
            | MatchCase(_, _, case2) ->
                matchPattern2.CreateSynMatchClause(case2.ToUntyped())
            | _ -> matchPattern2.CreateSynMatchClause(case2.ToUntyped())

        Some(clause1, Some clause2, rest)
    | _ -> None


let (|Equality|_|) (expr: FSharpMemberOrFunctionOrValue) =
    if expr.CompiledName = "op_Equality" then Some() else None

let (|EqualityCall|_|) =
    function
    | Call(None, Equality, [], _, [ Value(_); Const(v, t) ]) -> Some(v)
    | _ -> None

let test (a: obj) = ignore a

let mkRange (r: Text.Range) =
    Range.mkRange r.FileName (Position.mkPos r.StartLine (r.StartColumn)) (Position.mkPos r.EndLine (r.EndColumn))

let mkRange2 a b c d =
    Range.mkRange "garb.fsx" (Position.mkPos a b) (Position.mkPos c d)


type System.Object with
    member this.ToSynConst() =
        match this with
        | :? unit -> SynConst.Unit
        | :? bool as b -> SynConst.Bool b
        | :? sbyte as b -> SynConst.SByte b
        | :? byte as b -> SynConst.Byte b
        | :? int16 as i -> SynConst.Int16 i
        | :? int32 as i -> SynConst.Int32 i
        | :? int64 as i -> SynConst.Int64 i
        | :? uint64 as i -> SynConst.UInt64 i
        | :? string as s -> SynConst.String(s, SynStringKind.Regular, Range.Zero)
        | :? single as b -> SynConst.Single b
        | :? double as b -> SynConst.Double b
        | :? char as b -> SynConst.Char b
        | :? Decimal as b -> SynConst.Decimal b
        | :? (byte array) as arr -> SynConst.Bytes(arr, SynByteStringKind.Regular, Range.Zero)
        | _ -> failwith ""

type SynExprMatchTrivia with
    static member Empty: SynExprMatchTrivia = {
        SynExprMatchTrivia.WithKeyword = range.Zero
        SynExprMatchTrivia.MatchKeyword = range.Zero
    }

type SynBindingReturnInfoTrivia with
    static member Empty: SynBindingReturnInfoTrivia = {
        SynBindingReturnInfoTrivia.ColonRange = None
    }


type String with
    member this.Named() : SynPat =
        SynPat.Named(this.Replace("@", "").SynIdent(), false, None, Range.Zero)

    member this.Typed(typ: SynType) = this.Named().TypedPat(typ)

    member this.Typed(typ: FSharpType) = this.Named().TypedPat(typ.ToSynType())

    member this.Ident() : Ident =
        Ident(this.Replace("@", ""), Range.Zero)

    member this.SynIdent() : SynIdent = SynIdent(this.Ident(), None)


    member this.LongIdentWithDots() =
        let typeName: LongIdent =
            this
                //todo solve nicer
                .Replace("`1", "")
                .Replace("`2", "")
                .Replace("`3", "")
                .Replace("`4", "")
                .Replace("`5", "")
                .Replace("`6", "")
                .Replace("`7", "")
                .Split(".")
            |> Array.map (_.Ident())
            |> List.ofArray

        if typeName.Length = 1 then
            SynLongIdent(typeName, [], [])
        else
            let dotRanges = [
                for _ = 1 to typeName.Length - 1 do
                    yield Range.Zero
            ]

            SynLongIdent(typeName, dotRanges, [])

    member this.LongIdent() =
        SynType.LongIdent(this.LongIdentWithDots())

    member this.RecordFieldName() =
        RecordFieldName(this.LongIdentWithDots(), false)

    member this.Var() =
        SynType.Var(SynTypar(this.Ident(), TyparStaticReq.None, false), Range.Zero)

    member this.Const() =
        SynExpr.Const(SynConst.String(this, SynStringKind.Regular, Range.Zero), Range.Zero)

    member this.IdentExpr() = SynExpr.Ident(this.Ident())

    member this.IdentPat(args: FSharpMemberOrFunctionOrValue list) =
        if List.isEmpty args then
            this.Named()
        else
            SynPat.LongIdent(
                this.LongIdentWithDots(),
                None,
                None,
                // None,
                SynArgPats.Pats(args |> List.map (_.GetArgs())),
                None,
                Range.Zero
            )
    //SynPat.Named(Ident.ofString bindingName, false, None, range)

    member this.LongIdentExpr() =
        SynExpr.LongIdent(false, this.LongIdentWithDots(), None, Range.Zero)

type SynPat with
    member this.TypedPat(typ) = SynPat.Typed(this, typ, range.Zero)

    //todo this is sus
    member headPat.SynBinding(expr: SynExpr, ?isMutable) =
        //failwithf "%A" expr
        SynBinding(
            None,
            SynBindingKind.Normal,
            false,
            isMutable |> Option.defaultValue false,
            [],
            PreXmlDoc.Empty,
            valData = SynValData(None, SynValInfo([], SynArgInfo([], false, None)), None),
            headPat = headPat,
            returnInfo = None,
            expr = expr,
            range = range.Zero,
            debugPoint = DebugPointAtBinding.NoneAtInvisible,
            trivia = {
                LeadingKeyword = SynLeadingKeyword.Let(range.Zero)
                InlineKeyword = None
                EqualsRange = Some(range.Zero)
            }
        )

    member pat.CreateSynMatchClause((_, result): _ * FSharpExpr, ?whenExpr) =
        pat.CreateSynMatchClause(result.ToUntyped(), ?whenExpr = whenExpr)

    member pat.CreateSynMatchClause(result: SynExpr, ?whenExpr) =
        SynMatchClause(
            pat = pat,
            whenExpr = whenExpr,
            resultExpr = result,
            range = range.Zero,
            debugPoint = DebugPointAtTarget.No,
            trivia = {
                ArrowRange = Some(range.Zero)
                BarRange = Some(range.Zero)
            }
        )

type SynExpr with

    member expr.WrapInParens() =
        //don't wrap null/unit in parens, it gets weird
        match expr with
        | SynExpr.Tuple(_, [ SynExpr.Const(SynConst.Unit, _) ], _, _) -> SynExpr.Const(SynConst.Unit, Range.Zero)
        //| SynExpr.Null _
        | SynExpr.Const(SynConst.Unit, _) -> expr
        | _ -> SynExpr.Paren(expr, range.Zero, Some range.Zero, range.Zero)

    member expr.LambdaExpr(arg) = expr.LambdaExpr([ arg ])

    member this.Apply(args: SynExpr) =
        SynExpr.App(
            flag = ExprAtomicFlag.NonAtomic,
            isInfix = false,
            funcExpr = this,
            argExpr = args,
            range = range.Zero
        )

    member this.ApplyInfix(ar: SynExpr) =
        SynExpr.App(flag = ExprAtomicFlag.NonAtomic, isInfix = true, funcExpr = this, argExpr = ar, range = range.Zero)

    member this.ApplyInfix(args: SynExpr list) =
        match args with
        | [] -> this
        | [ x ] -> x.Apply(this)
        | x :: xs ->
            SynExpr
                .App(flag = ExprAtomicFlag.NonAtomic, isInfix = true, funcExpr = this, argExpr = x, range = range.Zero)
                .ApplyInfix(xs)

    member this.Apply(args: string) = this.Apply(args.Const())

    member this.Apply(args: SynExpr list) = this.Apply(args.Tuple())

    member this.Apply(args: FSharpExpr list) =
        match args with
        | [] -> this
        | [ arg ] -> this.Apply(arg.ToUntyped().WrapInParens())
        | args -> this.Apply(args.Tuple().WrapInParens())

    member this.Apply() =
        this.Apply(SynExpr.Const(SynConst.Unit, Range.Zero))

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

    member this.WithTypeArgs(fsTypes: ICollection<FSharpType>) =
        if fsTypes.Count > 0 then
            this.WithTypeArgs(fsTypes |> Seq.map (_.ToSynType()) |> Seq.toList)
        else
            this

    member this.WithTypeArgs(fsTypes: list<FSharpType>) =
        if fsTypes.Length > 0 then
            this.WithTypeArgs(fsTypes |> List.map (_.ToSynType()))
        else
            this


    member expr.RequireParens() =
        match expr with
        | SynExpr.Lambda _ -> true
        | _ -> false

    member this.AppendIdent(f: FSharpMemberOrFunctionOrValue) =
        let name =
            if f.IsPropertyGetterMethod then
                f.LogicalName.Replace("get_", "")
            else
                f.LogicalName

        let i = name.Ident()

        match this with
        | SynExpr.LongIdent(_, SynLongIdent(ids, _, _), _, _) ->
            let dotRanges = [
                for _ = 1 to ids.Length do
                    Range.Zero
            ]

            SynExpr.LongIdent(false, SynLongIdent(ids @ [ i ], dotRanges, []), None, Range.Zero)

        | SynExpr.Ident ident ->
            SynExpr.LongIdent(false, SynLongIdent([ ident; i ], [ Range.Zero ], []), None, Range.Zero)
        | _ ->
            let idents = [ "Failed"; "To"; "Build"; "Ident" ].LongIdentWithDots()
            SynExpr.LongIdent(false, idents, None, Range.Zero)

type String with
    member this.Apply(args: string) = this.IdentExpr().Apply(args)

    member this.Apply(args: SynExpr) = this.IdentExpr().Apply(args)

[<ExtensionAttribute>]
type ListExtensions =
    [<Extension>]
    static member LongIdentWithDots(args: string list) =
        SynLongIdent(args |> List.map (_.Replace("@", "").Ident()), [ Range.Zero ], [])

    [<Extension>]
    static member LongIdent(this: string list) =
        SynExpr.LongIdent(false, this.LongIdentWithDots(), None, Range.Zero)

    [<Extension>]
    static member Tuple(this: SynExpr list) =
        match this with
        | [] -> 
            SynExpr.Const(SynConst.Unit, Range.Zero)
        | this ->
            let commaRanges = [
                for _ = 1 to this.Length - 1 do
                    Range.Zero
            ]

            SynExpr.Tuple(false, this, commaRanges, Range.Zero)

    [<Extension>]
    static member LambdaExpr(this: SynExpr, args: SynPat list) =
        SynExpr.Lambda(
            fromMethod = false,
            inLambdaSeq = false,
            args = SynSimplePats.SimplePats([], [], range.Zero), //dunno about this
            //arrow = Some (range.Zero),
            body = this,
            parsedData = Some(args, this),
            range = Range.Zero,
            trivia = { ArrowRange = Some(range.Zero) }
        )

    [<Extension>]
    static member LambdaExpr(this: SynExpr, arg: SynPat) = this.LambdaExpr([ arg ])

    [<Extension>]
    static member Tuple(args: FSharpExpr list) : SynExpr =
        if args.Length = 1 then
            args[0].ToUntyped()
        else
            args
            |> List.map (_.ToUntyped())
            |> List.mapi (fun i s ->
                //having a lambda in a tupled call requires parens, or the comma will be
                //interpreted as a tuple in the lambda. Work without if the lambda is the last arg
                //since then we wont have any trailing comma.
                if s.RequireParens() && i <> args.Length - 1 then
                    s.WrapInParens()
                else
                    s)
            |> _.Tuple()

type SynType with
    member this.TypeArgs(genericTypes) =
        SynType.App(this, Some(Range.Zero), List.ofSeq genericTypes, [], Some(Range.Zero), false, Range.Zero)

    member this.TypeArgs(fsTypes: ICollection<FSharpType>) =
        if fsTypes.Count > 0 then
            this.TypeArgs(fsTypes |> Seq.map (_.ToSynType()))
        else
            this

    member this.TypeArgs(fsTypes: list<FSharpType>) =
        if fsTypes.Length > 0 then
            this.TypeArgs(fsTypes |> List.map (_.ToSynType()))
        else
            this

    member this.New(args) =
        SynExpr.New(false, this, args, range.Zero)

    member this.New(exprs: FSharpExpr list) =
        let argsToCtor =
            if List.isEmpty exprs then
                SynExpr.Const(SynConst.Unit, range.Zero)
            else
                exprs.Tuple().WrapInParens()

        this.New(argsToCtor)

module List =
    let intersperse element (list: 'a list) =
        match list with
        | [] -> []
        | x :: xs -> [
            yield x
            for x in xs do
                yield element
                yield x
          ]

type FSharpType with
    member fullType.Tuple() =
        let types = [
            for t in fullType.GenericArguments -> SynTupleTypeSegment.Type(t.ToSynType())
        ]

        SynType.Tuple(
            fullType.IsStructTupleType,
            types |> List.intersperse (SynTupleTypeSegment.Star(Range.Zero)),
            Range.Zero
        )

    member fsType.ToSynType() : SynType =
        if fsType.IsGenericParameter then
            fsType.GenericParameter.FullName.Var()
        elif fsType.HasTypeDefinition then
            fsType.TypeDefinition.DisplayName.LongIdent().TypeArgs(fsType.GenericArguments)
        elif (fsType.IsTupleType) then
            fsType.Tuple()
        elif (fsType.IsFunctionType) then
            SynType.Fun(
                fsType.GenericArguments[0].ToSynType(),
                fsType.GenericArguments[1].ToSynType(), //does currying solve this?
                Range.Zero,
                trivia = { ArrowRange = Range.Zero }
            )
        else
            "todo".LongIdent()

type FSharpMemberOrFunctionOrValue with
    member this.Named() =
        SynPat.Named(this.LogicalName.SynIdent(), this.IsMemberThisValue, None, Range.Zero)

    member this.GetArgs() =
        if
            this.FullType.HasTypeDefinition
            && this.FullType.TypeDefinition.DisplayName = "unit"
        then
            SynPat.Const(SynConst.Unit, Range.Zero)
        elif (this.FullType.HasTypeDefinition) then
            SynPat.Paren(this.LogicalName.Typed(this.FullType), Range.Zero)
        elif (this.FullType.IsTupleType) then
            SynPat.Paren(this.LogicalName.Typed(this.FullType), Range.Zero)
        else
            this.LogicalName.Named()

    member f.LongIdent() =
        if f.ApparentEnclosingEntity.IsFSharpModule then
            //static call, no args
            if
                f.ApparentEnclosingEntity.HasAttribute<AutoOpenAttribute>()
                || f.ApparentEnclosingEntity.AllCompilationPaths.IsEmpty
            then
                f.LogicalName.LongIdentExpr()
            else
                [ f.ApparentEnclosingEntity.DisplayName; f.LogicalName ].LongIdent()
        else if f.IsPropertyGetterMethod then
            [ f.ApparentEnclosingEntity.CompiledName; f.CompiledName.Replace("get_", "") ]
                .LongIdent()
        else
            [ f.ApparentEnclosingEntity.CompiledName; f.CompiledName ].LongIdent()


type FSharpExpr with
    member this.ToUntyped() : SynExpr =
        match this with
        | Application(expr, _types, []) ->
            expr.ToUntyped()
        | Application(expr, _types, args) ->
            let app = expr.ToUntyped()

            if app.RequireParens() then
                app.WrapInParens().Apply(args.Tuple())
            else
                app.Apply(args.Tuple())
        | Lambda(args, expr) -> expr.ToUntyped().LambdaExpr(args.GetArgs())
        | Const(c, _a) -> SynExpr.Const(c.ToSynConst(), Range.Zero)

        | Let((letBoundName, ex1, dbg: Syntax.DebugPointAtBinding), ex2) ->
            let inKeyword =
                match dbg with
                | Syntax.DebugPointAtBinding.Yes(r) -> mkRange r |> Some
                | _ -> None

            let r = Option.defaultValue range.Zero inKeyword
            let binding = ex1.ToUntyped()

            SynExpr.LetOrUse(
                false,
                false,
                bindings = [ letBoundName.Named().SynBinding(binding, letBoundName.IsMutable) ],
                body = ex2.ToUntyped(), //unit
                range = r,
                trivia = { InKeyword = inKeyword }
            )
        | NewUnionCase(t, case, expr) -> [ t.TypeDefinition.DisplayName; case.CompiledName ].LongIdent().Apply(expr)

        | Value value -> value.LogicalName.IdentExpr()
        | TupleGet(t, index, (Value value)) ->
            //tupledArg.Item1 is technically correct, but does not work in fsharp
            //so we rewrite it as a destructured let-binding instead. ugly, but seems to work.

            //in theory, we could destructure it earlier, but it requires forward parsing due to
            //name resolution, so leave as is for now.
            let totalLength = t.GenericArguments.Count - 1

            let elementPats = [
                for i in 0..totalLength ->
                    if i = index then
                        "value".IdentPat([])
                    else
                        SynPat.Wild(Range.Zero)
            ]

            let tuple =
                SynPat.Tuple(false, elementPats, [ for _ in 0 .. totalLength - 1 -> Range.Zero ], Range.Zero)

            SynExpr.LetOrUse(
                isRecursive = false,
                isUse = false,
                bindings = [ SynPat.Paren(tuple, Range.Zero).SynBinding(value.LogicalName.IdentExpr()) ],
                body = "value".IdentExpr(),
                range = range.Zero,
                trivia = { InKeyword = Some Range.Zero }
            )

        | TupleGet(t, index, UnionCaseGet(Value value, _, _, _)) ->
            //tupledArg.Item1 is technically correct, but does not work in fsharp
            //so we rewrite it as a destructured let-binding instead. ugly, but seems to work.

            //in theory, we could destructure it earlier, but it requires forward parsing due to
            //name resolution, so leave as is for now.
            let totalLength = t.GenericArguments.Count - 1

            let elementPats = [
                for i in 0..totalLength ->
                    if i = index then
                        "value".IdentPat([])
                    else
                        SynPat.Wild(Range.Zero)
            ]

            let tuple =
                SynPat.Tuple(false, elementPats, [ for _ in 0 .. totalLength - 1 -> Range.Zero ], Range.Zero)

            SynExpr.LetOrUse(
                isRecursive = false,
                isUse = false,
                bindings = [ SynPat.Paren(tuple, Range.Zero).SynBinding(value.LogicalName.IdentExpr()) ],
                body = "value".IdentExpr(),
                range = range.Zero,
                trivia = { InKeyword = Some Range.Zero }
            )

        | NewTuple(_, exprs) -> exprs.Tuple()
        | Coerce(fsType, fsExpr) -> SynExpr.Upcast(fsExpr.ToUntyped(), fsType.ToSynType(), Range.Zero)

        | TypeLambda(_, expr) ->
            let bod = expr.ToUntyped()
            SynExpr.LetOrUse(false, false, [], bod, range.Zero, { InKeyword = Some Range.Zero })

        | Sequential(ex1, ex2) ->
            let dbg = DebugPointAtSequential.SuppressBoth
            let e1 = ex1.ToUntyped()
            let e2 = ex2.ToUntyped()
            SynExpr.Sequential(dbg, false, e1, e2, Range.Zero, SynExprSequentialTrivia.Zero)

        | ValueSet(value, expr) ->
            SynExpr.LongIdentSet(value.LogicalName.LongIdentWithDots(), expr.ToUntyped(), range.Zero)
        | DefaultValue expr ->
            if expr.TypeDefinition.IsValueType then
                expr.TypeDefinition.FullName.LongIdent().TypeArgs(expr.GenericArguments).New([])
            else
                SynExpr.Null(Range.Zero)

        | NewObject(f, types, exprs) ->
            let ident = f.ApparentEnclosingEntity.FullName.LongIdent()
            let typeArgs = ident.TypeArgs(types)
            let dbug = typeArgs.New(exprs)

            dbug

        | IfThenElse(ifExpr, thenExpr, elseExpr) ->
            let debugPoint = DebugPointAtBinding.NoneAtInvisible

            let trivia = {
                SynExprIfThenElseTrivia.IsElif = false
                IfKeyword = range.Zero
                ThenKeyword = range.Zero
                ElseKeyword = Some range.Zero
                IfToThenRange = range.Zero
            }

            SynExpr.IfThenElse(
                ifExpr.ToUntyped(),
                thenExpr.ToUntyped(),
                Some(elseExpr.ToUntyped()),
                debugPoint,
                isFromErrorRecovery = false,
                range = range.Zero,
                trivia = trivia
            )

        | Quote(expr) ->
            let inner = expr.ToUntyped()
            SynExpr.Quote(inner, false, inner, false, range.Zero)

        | NewAnonRecord(t, exprs) ->
            let records = [
                for (m, r) in exprs |> Seq.zip t.AnonRecordTypeDetails.SortedFieldNames ->
                    (m.LongIdentWithDots(), Some(Range.Zero), r.ToUntyped())
            ]

            SynExpr.AnonRecd(
                false,
                copyInfo = None,
                recordFields = records,
                range = Range.Zero,
                trivia = { OpeningBraceRange = Range.Zero }
            )
        | NewRecord(t, exprs) ->
            match t.BaseType with
            | Some b when  b.BasicQualifiedName = "Microsoft.FSharp.Core.exn" -> 
                //maybe qualify, but rather not.
                [ t.TypeDefinition.CompiledName ].LongIdent().Apply(exprs)
            | _ ->
                let records = [
                    for (m, r) in exprs |> Seq.zip t.TypeDefinition.FSharpFields do
                        SynExprRecordField.SynExprRecordField(
                            fieldName = m.Name.RecordFieldName(),
                            equalsRange = Some(Range.Zero),
                            expr = Some(r.ToUntyped()),
                            blockSeparator = None
                        )
                ]

                SynExpr.Record(baseInfo = None, copyInfo = None, recordFields = records, range = Range.Zero)
        | UnionCaseGet(_expr, _typ, _case, field) -> field.Name.IdentExpr()

        | FSharpFieldGet(Some(Value caller), _typ, field: FSharpField) ->
            [ caller.CompiledName; field.Name ].LongIdent()

        | Call(Some(callingEntity), f, _, genericArgs, args) ->
            if List.isEmpty args && not f.IsPropertyGetterMethod then
                //not a getter means we compile in a '()' to the apply.
                callingEntity.ToUntyped().AppendIdent(f).WithTypeArgs(genericArgs).Apply()
            else
                callingEntity.ToUntyped().AppendIdent(f).Apply(args)

        //operators
        | Call(None, f: FSharpMemberOrFunctionOrValue, _, _, args) when f.CompiledName.StartsWith("op_") ->
            let funcExpr = f.DisplayName.Replace("(", "").Replace(")", "").LongIdentExpr()

            match args with
            | [ a ] -> funcExpr.ApplyInfix(a.ToUntyped())
            | args -> funcExpr.ApplyInfix(args |> List.map (_.ToUntyped()) |> List.rev)

        //fsharp function calls
        | Call(None, f, _, _, args) when f.CurriedParameterGroups.Count > 1 ->
            args
            |> List.map (_.ToUntyped())
            |> List.fold (_.Apply) (f.FullName.LongIdentExpr())

        | Call(None, f, _, (_ :: _ as genericArgs), []) ->
            //with no args, we need generic args, since they can never be infered.
            f.LongIdent().WithTypeArgs(genericArgs).Apply()
        //basic calls
        | Call(None, f, _, _genericArgs, args) ->
            if f.IsPropertyGetterMethod then
                f.LongIdent()
            else
                f.LongIdent().Apply(args)

        | DecisionTree(ifElse, nodes) ->
            let clauses = ifElse.GetSynMatchClauses(nodes)

            SynExpr.Match(
                //  range.Zero,
                DebugPointAtBinding.NoneAtInvisible,
                SynExpr.Ident(Ident(ifElse.GetMatchName(), range.Zero)),
                clauses,
                range.Zero,
                SynExprMatchTrivia.Empty

            )
        | a -> "invalidArg".Apply("fsharpExpr").Apply(sprintf "%A." a)

    member fsharpExpr.GetSynMatchClauses(nodes: (_ * FSharpExpr) list) = [
        match fsharpExpr with

        | MatchUnionCase nodes (clause1, Some clause2, rest2) ->
            yield clause1
            yield clause2
            yield! rest2.GetSynMatchClauses(nodes)

        | MatchUnionCase nodes (clause, None, rest) ->
            yield clause
            yield! rest.GetSynMatchClauses(nodes)

        //if we end up in a wildcard, with an inner match, we assume the match must fail here
        //or it would be caught above the wildcard.
        //slightly risky.
        | WildCardCase nodes (f, n1, MatchCase(_, _, n2)) ->
            yield SynPat.Wild(range.Zero).CreateSynMatchClause(n1.ToUntyped(), f.ToUntyped())
            yield SynPat.Wild(range.Zero).CreateSynMatchClause(n2.ToUntyped())

        | DirectMatch nodes (EqualityCall v, n1, rest) ->
            test v
            yield v.ToString().Named().CreateSynMatchClause(n1.ToUntyped())
            yield! rest.GetSynMatchClauses(nodes)

        | Jump nodes (_, e) -> yield! e.GetSynMatchClauses(nodes)
        | Let((names, UnionCaseGet(_,_,c,_), _), result) -> 
            //if we have a unioncase get here, we can match against it
            yield c.LongIdent([ names.CompiledName ]).CreateSynMatchClause(result.ToUntyped())

        | Let((names, _, _), result) -> 
            //for consts
            yield names.CompiledName.Named().CreateSynMatchClause(result.ToUntyped())

        //see if we can find an abstraction for this...
        | IfThenElse(Call(_,guard,_,_,_) as f, (Jump nodes (_, n1)), (Jump nodes (_, Tree(g, a, b, c, d)))) -> 
            test guard
            yield SynPat.Wild(range.Zero).CreateSynMatchClause(n1.ToUntyped(), f.ToUntyped())
            yield SynPat.Wild(range.Zero).CreateSynMatchClause(b.ToUntyped())

        | a -> 
            test a
            yield SynPat.Wild(range.Zero).CreateSynMatchClause(a.ToUntyped())
    ]

    member fsharpExpr.GetMatchName() : string =
        match fsharpExpr with
        | IfThenElse(Call(None, Equality, [], _, [ Value(expr); Const(_, _) ]), DecisionTreeSuccess(_, _), _)
        | IfThenElse(UnionCaseTest(Value expr, _, _), _, _) -> expr.CompiledName
        | _ -> "failed to find match name"

type FSharpUnionCase with
    member unionCase.LongIdent(?names: string list) : SynPat =
        let names = names |> Option.defaultValue []

        let synPat =
            if List.isEmpty names then
                //dont generate a wildcard for unions with 0 fields.
                if unionCase.Fields.Count = 0 then
                    []
                else
                    [ SynPat.Wild(Range.Zero) ]
            else
                let commaRanges = List.init (names.Length - 1) (fun _ -> Range.Zero)

                let elements = names |> List.map _.Named()

                [
                    SynPat.Paren(SynPat.Tuple(false, elements, commaRanges, range.Zero), range.Zero)
                ]

        SynPat.LongIdent(
            longDotId = unionCase.CompiledName.LongIdentWithDots(),
            //propertyKeyword = None,
            extraId = None,
            typarDecls = None,
            argPats = SynArgPats.Pats(synPat),
            accessibility = None,
            range = range.Zero
        )

    member pat.CreateSynMatchClause((_, result): _ * FSharpExpr, ?whenExpr) =
        pat.CreateSynMatchClause(result.ToUntyped(), ?whenExpr = whenExpr)

    member pat.CreateSynMatchClause(result: SynExpr, ?whenExpr) =
        let pattern = pat.LongIdent()
        pattern.CreateSynMatchClause(result, ?whenExpr = whenExpr)
