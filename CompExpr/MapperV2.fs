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
open System.Collections.Generic

type String with
    member this.Named() =
        SynPat.Named(this.Replace("@", "").Ident(), false, None, Range.Zero)

    member this.Typed(typ: SynType) =
        this.Named().TypedPat(typ)

    member this.Typed(typ: FSharpType) =
        this.Named().TypedPat(typ.ToSynType())

    member this.Ident() = Ident(this.Replace("@", ""), Range.Zero)

    member this.LongIdentWithDots() =
        let typeName =
            this.Replace("`1", "").Split(".") |> Array.map (_.Ident()) |> List.ofArray
        LongIdentWithDots(typeName, [])

    member this.LongIdent() =
        SynType.LongIdent(this.LongIdentWithDots())

    member this.RecordFieldName() =
        RecordFieldName(this.LongIdentWithDots(), false)

    member this.Var() =
        SynType.Var(SynTypar(this.Ident(), TyparStaticReq.None, false), Range.Zero)

    member this.Const() =
        SynExpr.Const(SynConst.String(this, SynStringKind.Regular, Range.Zero), Range.Zero)

    member this.IdentExpr() =   
        SynExpr.Ident(this.Ident())

    member this.LongIdentExpr() =
        SynExpr.LongIdent(false, this.LongIdentWithDots(), None, Range.Zero)

type SynPat with
    member this.TypedPat(typ) = 
        SynPat.Typed(this, typ, Range.Zero)

type SynExpr with

    member expr.WrapInParens() =
        //don't wrap null/unit in parens, it gets weird
        match expr with
        | SynExpr.Tuple(_, [ SynExpr.Const(SynConst.Unit,_) ], _, _)
            -> SynExpr.Const(SynConst.Unit, Range.Zero)
        | SynExpr.Null _
        | SynExpr.Const(SynConst.Unit,_)
            -> expr
        | _ ->
            SynExpr.Paren(expr, range.Zero, Some range.Zero, range.Zero)

    member expr.LambdaExpr(arg) = expr.LambdaExpr([ arg ])

    member this.Apply(args: SynExpr) =
        SynExpr.App(
            flag = ExprAtomicFlag.NonAtomic,
            isInfix = false,
            funcExpr = this,
            argExpr = args,
            range = range.Zero
        )

    member this.ApplyInfix(args: SynExpr) =
        SynExpr.App(
            flag = ExprAtomicFlag.Atomic,
            isInfix = true,
            funcExpr = this,
            argExpr = args,
            range = range.Zero
        )

    member this.Apply(args: string) =
        this.Apply(args.Const())

    member this.Apply(args: SynExpr list) =
        this.Apply(args.Tuple())

    member this.Apply(args: FSharpExpr list) =
        match args with
        | [ arg ] ->
            this.Apply(arg.ToUntyped().WrapInParens())
        | args ->
            let (t: SynExpr) = args.Tuple()
            this.Apply(t.WrapInParens())

    member this.Apply() = this.Apply(SynExpr.Const(SynConst.Unit, Range.Zero))

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
        | SynExpr.Lambda _ ->
            true
        | _ -> false

    member this.AppendIdent(idents: string list) =
        match this with
        | SynExpr.LongIdent (_,LongIdentWithDots(ids, _),_,_) -> 
            let (LongIdentWithDots(moreIds,_)) = idents.LongIdentWithDots()
            SynExpr.LongIdent(false, LongIdentWithDots(ids @ moreIds, []), None, Range.Zero)
        | SynExpr.Ident ident -> 
            let (LongIdentWithDots(moreIds,_)) = idents.LongIdentWithDots()
            SynExpr.LongIdent(false, LongIdentWithDots([ ident ] @ moreIds, []), None, Range.Zero)
        | _ -> failwith ""

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
                SynExpr.Const(SynConst.Unit, Text.range.Zero)
            else
                exprs.Tuple().WrapInParens()
        this.New(argsToCtor)

type FSharpType with
    member fullType.Tuple() =
        SynType.Tuple(
            fullType.IsStructTupleType,
            [ for _ in fullType.GenericArguments -> false, SynType.Anon(Range.Zero ) ],
            Range.Zero 
        )

    member fsType.ToSynType () : SynType =
        if fsType.IsGenericParameter then
            fsType.GenericParameter.FullName.Var()
        elif fsType.HasTypeDefinition then
            fsType.TypeDefinition.DisplayName.LongIdent().TypeArgs(fsType.GenericArguments)
        elif (fsType.IsTupleType) then
            //todo
            fsType.Tuple()
        else
            "todo".LongIdent()

    member fullType.ToSynPat (logicalName: string) =
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
        if this.FullType.HasTypeDefinition && this.FullType.TypeDefinition.DisplayName = "unit" then
            SynPat.Const(SynConst.Unit, Range.Zero)
        else
            SynPat.Paren(this.FullType.ToSynPat this.LogicalName, Range.Zero)
            
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


    member f.LongIdent() =
        if f.ApparentEnclosingEntity.IsFSharpModule then
            //static call, no args
            if f.ApparentEnclosingEntity.HasAttribute<AutoOpenAttribute>() then
                //caller.ApparentEnclosingEntity
                f.LogicalName.LongIdentExpr()
            else
                [ f.ApparentEnclosingEntity.CompiledName; f.LogicalName ].LongIdent()
        else
            if f.IsPropertyGetterMethod then
                [ f.ApparentEnclosingEntity.CompiledName; f.CompiledName.Replace("get_", "") ].LongIdent()
            else
                
                [ f.ApparentEnclosingEntity.CompiledName; f.CompiledName ].LongIdent()
                

type FSharpExpr with
    member this.ToUntyped(): SynExpr =
        match this with
        | Application(Value _ as expr, types, [ arg ]) ->
            let x = expr.ToUntyped()
            x.Apply(arg.ToUntyped())

        | Application(expr, types, [ arg ]) ->
            let x = expr.ToUntyped().WrapInParens()
            x.Apply(arg.ToUntyped())

           //expr.ToUntyped().Apply(arg.ToUntyped())
        | Application(expr, types, args) ->
            expr.ToUntyped().WrapInParens().Apply(args.Tuple())
        | Lambda(args, expr) ->
            let l = expr.ToUntyped()
            l.LambdaExpr(args.getArgs())
        | Const(c, _) ->
            match c with
            | :? unit -> SynExpr.Const(SynConst.Unit, Range.Zero)
            | :? bool as b -> SynExpr.Const(SynConst.Bool b, Range.Zero)
            | :? sbyte as b -> SynExpr.Const(SynConst.SByte b, Range.Zero)
            | :? byte as b -> SynExpr.Const(SynConst.Byte b, Range.Zero)
            | :? int16 as i -> SynExpr.Const(SynConst.Int16 i, Range.Zero)
            | :? int32 as i -> SynExpr.Const(SynConst.Int32 i, Range.Zero)
            | :? int64 as i -> SynExpr.Const(SynConst.Int64 i, Range.Zero)
            | :? uint64 as i -> SynExpr.Const(SynConst.UInt64 i, Range.Zero)

            | :? string as s -> s.Const()

            | :? single as b -> SynExpr.Const(SynConst.Single b, Range.Zero)
            | :? double as b -> SynExpr.Const(SynConst.Double b, Range.Zero)
            | :? char as b -> SynExpr.Const(SynConst.Char b, Range.Zero)
            | :? Decimal as b -> SynExpr.Const(SynConst.Decimal b, Range.Zero)

            | _ -> failwith ""

        | Let((a, ex1, dbg: DebugPointAtBinding), ex2) ->
            let inKeyword =
                match dbg with
                | DebugPointAtBinding.Yes a -> Some a
                | _ -> None

            SynExpr.LetOrUse(
                false,
                false,
                bindings = [ a.createBinding(ex1.ToUntyped()) ],
                body = ex2.ToUntyped(),
                range = Option.defaultValue Range.Zero inKeyword,
                trivia = { InKeyword = inKeyword }
            )
        //failwith ""
        | NewUnionCase(typ, case, []) ->
            [
                typ.TypeDefinition.DisplayName
                case.CompiledName
            ].LongIdent().Apply([])

        | NewUnionCase(t, case, expr) ->
            let arg = expr |> List.map (_.ToUntyped()) |> _.Tuple()

            let ident =
                
                    [
                        t.TypeDefinition.DisplayName
                        case.CompiledName
                    ].LongIdent()
                

            arg.WrapInParens().ApplyTo(ident)

        | Value value -> value.LogicalName.IdentExpr()
        | TupleGet(b, index, (Value value)) -> 
            [ value.LogicalName; $"Item{(index + 1)}" ].LongIdent()

        | NewTuple(_, exprs) ->
            exprs.Tuple()
        | Coerce(fsType, fsExpr) ->
            SynExpr.Upcast(fsExpr.ToUntyped(), fsType.ToSynType(), Range.Zero)

        | TypeLambda(_, expr) ->
            let bod = expr.ToUntyped()
            SynExpr.LetOrUse(false, false, [], bod, Text.range.Zero, { InKeyword = Some Range.Zero })

        | Sequential(ex1, ex2) ->
            let dbg = DebugPointAtSequential.SuppressBoth
            let e1 = ex1.ToUntyped()
            let e2 = ex2.ToUntyped()
            SynExpr.Sequential(dbg, false, e1, e2, Range.Zero)

        | ValueSet(value, expr) ->
            SynExpr.LongIdentSet(
                value.LogicalName.LongIdentWithDots(),
                expr.ToUntyped(),
                Text.Range.Zero
            )
        | DefaultValue expr ->
            if expr.TypeDefinition.IsValueType then
                expr.TypeDefinition.FullName.LongIdent().TypeArgs(expr.GenericArguments).New(SynExpr.Const(SynConst.Unit, range.Zero))                
            else
                SynExpr.Null(Range.Zero)

        | NewObject(f, types, exprs) ->
            f.ApparentEnclosingEntity.FullName.LongIdent().TypeArgs(types).New(exprs)

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
        | NewRecord(t, exprs) ->
            let records = [
                for (m, r) in exprs |> Seq.zip t.TypeDefinition.FSharpFields do
                    SynExprRecordField.SynExprRecordField(
                        fieldName = m.Name.RecordFieldName(),
                        equalsRange = None,
                        expr = Some(r.ToUntyped()),
                        blockSeparator = None
                    )
            ]

            SynExpr.Record(None, None, records, range.Zero)
        | UnionCaseGet(expr, typ, case, field) ->
            field.Name.IdentExpr()

        | FSharpFieldGet(Some(Value caller), typ, field: FSharpField) ->
            [ caller.CompiledName; field.Name ].LongIdent()

        | Call(Some(callingEntity), f, _, genericArgs, args) ->
            //eg Thing.do a, Thing.other a b, Thing.third (a, b), obj.Method(a, b) etc
            //todo handle issues with logicalName, such as get_
            if f.IsPropertyGetterMethod then
                callingEntity.ToUntyped().AppendIdent( [f.LogicalName.Replace("get_", "") ])
            else
                let argsToTheCall = 
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
                                s
                        )
                       // |> List.rev
                        |> _.Tuple()
                //.WithTypeArgs(genericArgs)
                callingEntity.ToUntyped().AppendIdent( [f.LogicalName ]).Apply(argsToTheCall.WrapInParens())
        //operators
        //fsharp function calls

        | Call(None, f, _, genericArgs, args) when f.CompiledName.StartsWith("op_") ->
            args 
            |> List.map (_.ToUntyped())
            |> List.fold (_.ApplyInfix) (f.CompiledName.LongIdentExpr())

        | Call(None, f, _, genericArgs, args) when f.CurriedParameterGroups.Count > 1 ->
            args 
            |> List.map (_.ToUntyped())            
            |> List.fold (_.Apply) (f.FullName.LongIdentExpr())

        | Call(None, f, _, [], []) -> 
            f.LongIdent()

        | Call(None, f, _, genericArgs, []) -> 
            //with no args, we need generic args, since they can never be infered.
            [ 
                f.ApparentEnclosingEntity.CompiledName
                f.CompiledName 
            ].LongIdent().WithTypeArgs(genericArgs).Apply()

        | Call(None, f, _, genericArgs, args) ->
             f.LongIdent().Apply(args)
            //[ 
            //    f.ApparentEnclosingEntity.CompiledName
            //    f.CompiledName  
            //].LongIdent().Apply(args)

        | a ->
            "invalidArg".Apply("fsharpExpr").Apply(sprintf "%A." a)