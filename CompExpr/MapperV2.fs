module rec CompExpr.MapperV2

open FSharp.Compiler
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Symbols.FSharpExprPatterns
// open FSharp.Compiler.Syntax
// open FSharp.Compiler.SyntaxTrivia
// open FSharp.Compiler.Text

open Fantomas.FCS.Syntax
open Fantomas.FCS.SyntaxTrivia
open Fantomas.FCS.Text

open System.Runtime.CompilerServices

open System
open Fantomas.FCS.Xml
open System.Collections.Generic

type SynExprMatchTrivia with
    static member Empty : SynExprMatchTrivia =
        { SynExprMatchTrivia.WithKeyword = range.Zero;
          SynExprMatchTrivia.MatchKeyword = range.Zero
         }
// type SynExprSequentialTrivia with
//     static member Empty : SynExprSequentialTrivia =
//         { SynExprSequentialTrivia.SeparatorRange = None;
//          }

type SynBindingReturnInfoTrivia with
    static member Empty : SynBindingReturnInfoTrivia =
        { SynBindingReturnInfoTrivia.ColonRange = None;
         }


type String with
    member this.Named(): SynPat =
        SynPat.Named(this.Replace("@", "").SynIdent(), false, None, Range.Zero)

    member this.Typed(typ: SynType) =
        this.Named().TypedPat(typ)

    member this.Typed(typ: FSharpType) =
        this.Named().TypedPat(typ.ToSynType())

    member this.Ident() : Ident = Ident(this.Replace("@", ""), Range.Zero)

    member this.SynIdent() : SynIdent = SynIdent(this.Ident(), None)


    member this.LongIdentWithDots() =
        let typeName: LongIdent =
            this.Replace("`1", "").Split(".") |> Array.map (_.Ident()) |> List.ofArray
        SynLongIdent( typeName, [], [])

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

    member this.IdentPat(args: FSharpMemberOrFunctionOrValue list) =
        if List.isEmpty args then
            this.Named()
        else
            SynPat.LongIdent(
                this.LongIdentWithDots(),
                None,
                None,
               // None,
                SynArgPats.Pats(args |> List.map (_.getArgs())),
                None,
                Range.Zero
            )
            //SynPat.Named(Ident.ofString bindingName, false, None, range)

    member this.LongIdentExpr() =
        SynExpr.LongIdent(false, this.LongIdentWithDots(), None, Range.Zero)

type SynPat with
    member this.TypedPat(typ) = 
        SynPat.Typed(this, typ, range.Zero)

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
            returnInfo = None,//Some(SynBindingReturnInfo(SynType.Anon(range.Zero), range.Zero, [], SynBindingReturnInfoTrivia.Empty)),
            expr = expr,
            range = range.Zero,
            debugPoint = DebugPointAtBinding.NoneAtInvisible,
            trivia = {
                LeadingKeyword = SynLeadingKeyword.Let(range.Zero)
                InlineKeyword = None
                EqualsRange = Some(range.Zero)
            }
        )
    member headPat.SynBinding2(expr: SynExpr, r) =
        //failwithf "%A" expr
        SynBinding(
            None,
            SynBindingKind.Normal,
            false,
            true,
            [],
            PreXmlDoc.Empty,
            valData = SynValData(None, SynValInfo([], SynArgInfo([], false, None)), None),
            headPat = headPat,
            returnInfo = None,//Some(SynBindingReturnInfo(SynType.Anon(range.Zero), range.Zero, [], SynBindingReturnInfoTrivia.Empty)),
            expr = expr,
            range = r,
            debugPoint = DebugPointAtBinding.NoneAtInvisible,
            trivia = {
                LeadingKeyword = SynLeadingKeyword.Let(range.Zero)
                InlineKeyword = None
                EqualsRange = Some(r)
            }
        )

    member pat.GetSynMatchClause((_, result): _ * FSharpExpr, ?whenExpr) =
        pat.GetSynMatchClause(result.ToUntyped(), ?whenExpr = whenExpr)

    member pat.GetSynMatchClause(result: SynExpr, ?whenExpr) =
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
        | SynExpr.Tuple(_, [ SynExpr.Const(SynConst.Unit,_) ], _, _)
            -> SynExpr.Const(SynConst.Unit, Range.Zero)
        //| SynExpr.Null _
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
        | [] -> 
            this
        | [ arg ] ->
            this.Apply(arg.ToUntyped().WrapInParens())
        | args ->
            this.Apply(args.Tuple().WrapInParens())

    member this.Apply() = this.Apply(SynExpr.Const(SynConst.Unit, Range.Zero))

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

    member this.AppendIdent(f: FSharpMemberOrFunctionOrValue) =
        let name = if f.IsPropertyGetterMethod then f.LogicalName.Replace("get_", "") else f.LogicalName
        let i = name.Ident()
        match this with
        | SynExpr.LongIdent (_, SynLongIdent(ids, _, _),_,_) -> 
            SynExpr.LongIdent(false, SynLongIdent(ids @ [i], [], [] ), None, Range.Zero)
        | SynExpr.Ident ident -> 
            SynExpr.LongIdent(false, SynLongIdent([ ident; i ], [], []), None, Range.Zero)
        | _ -> 
            let idents = [ "Failed"; "To"; "Build"; "Ident" ].LongIdentWithDots()
            SynExpr.LongIdent(false, idents, None, Range.Zero)

type String with
    member this.Apply(args: string) =
        this.IdentExpr().Apply(args)

    member this.Apply(args: SynExpr) =
        this.IdentExpr().Apply(args)

[<ExtensionAttribute>]
type ListExtensions =
    [<Extension>]
    static member LongIdentWithDots(args: string list) =
        SynLongIdent(args |> List.map (_.Replace("@", "").Ident()),  [ Range.Zero ], [])

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
            args = SynSimplePats.SimplePats([],[], range.Zero), //dunno about this
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
                    s
            )
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

type FSharpType with
    member fullType.Tuple() =
        SynType.Tuple(
            fullType.IsStructTupleType,
            [ for _ in fullType.GenericArguments -> 
                SynTupleTypeSegment.Slash(range.Zero ) ],
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

type FSharpMemberOrFunctionOrValue with
    member this.Named() =
        SynPat.Named(this.LogicalName.SynIdent(), this.IsMemberThisValue, None, Range.Zero)

    member this.getArgs () =
        if this.FullType.HasTypeDefinition && this.FullType.TypeDefinition.DisplayName = "unit" then
            SynPat.Const(SynConst.Unit, Range.Zero)
        elif (this.FullType.HasTypeDefinition) then
            SynPat.Paren(this.LogicalName.Typed(this.FullType), Range.Zero)
        elif (this.FullType.IsTupleType) then
            SynPat.Paren(this.LogicalName.Typed(this.FullType), Range.Zero)
        else
            this.LogicalName.Named() 

    // member value.createBinding body =
    //     value.Named().SynBinding2(body, value.IsMutable)


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
        | Application(expr, types, args) ->
            let app = expr.ToUntyped()
            if app.RequireParens() then
                app.WrapInParens().Apply(args.Tuple())
            else
                app.Apply(args.Tuple())
        | Lambda(args, expr) ->
            expr.ToUntyped().LambdaExpr(args.getArgs())
        | Const(c, _a) ->
            
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

        | Let((a, ex1, dbg: Syntax.DebugPointAtBinding), ex2) ->
            let inKeyword =
                match dbg with
                | Syntax.DebugPointAtBinding.Yes(r) -> 
                    //todo
                    let a =
                        Range.mkRange 
                            ""
                            (Position.mkPos r.StartLine (r.StartColumn))
                            (Position.mkPos r.EndLine (r.EndColumn))
                    //failwithf "%A\n%A" r a
                    a
                    |> Some
                | _ -> None
            let r = Option.defaultValue range.Zero inKeyword
            SynExpr.Const(SynConst.Int64(9L), r)
            //failwithf "%A" (r)
            // SynExpr.LetOrUse(
            //     false,
            //     false,
            //     bindings = [ 
            //         //a.Named().SynBinding2(ex1.ToUntyped(), r)
            //        // a.Named().SynBinding2(SynExpr.Const(SynConst.Int64(9L), range.Zero), r)
            //         SynBinding(
            //             None,
            //             SynBindingKind.Normal,
            //             false,
            //             true,
            //             [],
            //             PreXmlDoc.Empty,
            //             valData = SynValData(None, SynValInfo([], SynArgInfo([], false, None)), None),
            //             headPat = a.Named(),
            //             returnInfo = None,
            //             expr = SynExpr.Const(SynConst.Int64(9L), range.Zero),
            //             range = r,
            //             debugPoint = DebugPointAtBinding.Yes(r),
            //             trivia = {
            //                 LeadingKeyword = SynLeadingKeyword.Let(range.Zero)
            //                 InlineKeyword = None
            //                 EqualsRange = Some(Range.shiftStart 0 1 r)
            //             }
            //         )
            //         //a.createBinding(SynExpr.Const(SynConst.Unit, Option.defaultValue range.Zero inKeyword)) 
            //     ],
            //     body =  ex2.ToUntyped(), //unit
            //     range = r,
            //     trivia = { InKeyword = Some(range.Zero) }
            // )
        | NewUnionCase(t, case, expr) ->                
            [
                t.TypeDefinition.DisplayName
                case.CompiledName
            ].LongIdent().Apply(expr)
                
        | Value value -> value.LogicalName.IdentExpr()
        | TupleGet(b, index, (Value value)) -> 
            [ value.LogicalName; $"Item{(index + 1)}" ].LongIdent()

        | NewTuple(_, exprs) ->
            exprs.Tuple()
        | Coerce(fsType, fsExpr) ->
            SynExpr.Upcast(fsExpr.ToUntyped(), fsType.ToSynType(), Range.Zero)

        | TypeLambda(_, expr) ->
            let bod = expr.ToUntyped()
            SynExpr.LetOrUse(false, false, [], bod, range.Zero, { InKeyword = Some Range.Zero })

        | Sequential(ex1, ex2) ->
            let dbg = DebugPointAtSequential.SuppressBoth
            let e1 = ex1.ToUntyped()
            let e2 = ex2.ToUntyped()
            SynExpr.Sequential(dbg, false, e1, e2, Range.Zero)

        | ValueSet(value, expr) ->
            SynExpr.LongIdentSet(
                value.LogicalName.LongIdentWithDots(),
                expr.ToUntyped(),
                range.Zero
            )
        | DefaultValue expr ->
            if expr.TypeDefinition.IsValueType then
                expr.TypeDefinition.FullName.LongIdent().TypeArgs(expr.GenericArguments).New([])                
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
            if List.isEmpty args && not f.IsPropertyGetterMethod then
                //not a getter means we compile in a '()' to the apply.
                callingEntity.ToUntyped().AppendIdent(f).WithTypeArgs(genericArgs).Apply()
            else
                callingEntity.ToUntyped().AppendIdent(f).Apply(args)

        //operators
        | Call(None, f: FSharpMemberOrFunctionOrValue, _, _, args) when f.CompiledName.StartsWith("op_") ->
            //failwithf "%O" f.DisplayName
            args 
            |> List.map (_.ToUntyped())
            |> List.fold (_.ApplyInfix) (f.DisplayName.Replace("(", "").Replace(")", "").LongIdentExpr())

        //fsharp function calls
        | Call(None, f, _, _, args) when f.CurriedParameterGroups.Count > 1 ->
            args 
            |> List.map (_.ToUntyped())            
            |> List.fold (_.Apply) (f.FullName.LongIdentExpr())

        | Call(None, f, _, (_::_ as genericArgs), []) -> 
            //with no args, we need generic args, since they can never be infered.
            f.LongIdent().WithTypeArgs(genericArgs).Apply()
        //basic calls
        | Call(None, f, _, genericArgs, args) ->
            if f.IsPropertyGetterMethod then
                f.LongIdent()
            else
                f.LongIdent().Apply(args)

        | DecisionTree(ifElse, nodes) ->
            let clauses =  ifElse.GetSynMatchClauses(nodes, 0)

            SynExpr.Match(
              //  range.Zero,
                DebugPointAtBinding.NoneAtInvisible,
                SynExpr.Ident(Ident(ifElse.GetMatchName(), range.Zero)),
                clauses,
                range.Zero,
                SynExprMatchTrivia.Empty
                
            )
        | a ->
            "invalidArg".Apply("fsharpExpr").Apply(sprintf "%A." a)

    member fsharpExpr.GetSynMatchClauses(result: (_ * FSharpExpr) list, depth: int) = [
        match fsharpExpr with
        | DecisionTreeSuccess(i, _) ->
            
            SynPat.Wild(range.Zero).GetSynMatchClause(result[i])


        | IfThenElse(UnionCaseTest(Value _, typ, case), thenExpr, Call(_)) ->
            //match clause?
            case.LongIdent().GetSynMatchClause(thenExpr.ToUntyped())

        | IfThenElse(UnionCaseTest(Value expr, typ, case), DecisionTreeSuccess(i, _), rest) ->
            case.LongIdent().GetSynMatchClause(result[i])
            
            yield! rest.GetSynMatchClauses(result, depth + 1)           

        | IfThenElse(UnionCaseTest(Value expr, typ, case), IfThenElse(ifExpr, DecisionTreeSuccess(case1, _), DecisionTreeSuccess(case2, _)), rest) ->                               
            let letResult = ifExpr.LetOrUse(result[case1])
            //test.GetSynMatchClause(letResult, ifExpr.ToUntyped())
            case.LongIdent().GetSynMatchClause(letResult, ifExpr.ToUntyped())

            let (_, elseExpr) = result[case2]
            yield! elseExpr.GetSynMatchClauses(result, depth + 1)

            yield! rest.GetSynMatchClauses(result, depth + 1)  

        | _ -> failwith "unknown decision-tree option"
    ]

    member ifExpr.LetOrUse((_, thenExpr): _ * FSharpExpr) : SynExpr =
        SynExpr.LetOrUse(
            false,
            false,
            [ ifExpr.GetBoundName().Named().SynBinding(ifExpr.GetCaseName().IdentExpr()) ],
            (thenExpr.ToUntyped()),
            range.Zero,
            { InKeyword = Some(range.Zero) }            
        )


    member fsharpExpr.GetMatchName() : string =
        match fsharpExpr with
        | IfThenElse(UnionCaseTest(Value expr, _, _), _, _) -> expr.CompiledName
        | _ -> "failed to resolve name"

    member fsharpExpr.GetCaseName() : string =
        match fsharpExpr with
        | Let((_, UnionCaseGet(expr, typ, case, field), _), _) -> field.Name
        | _ -> "failed to get name of binding"

    member fsharpExpr.GetBoundName() : string =
        match fsharpExpr with
        | Let((v, _, _), _) -> v.CompiledName
        | _ -> "failed to get name of binding"

type FSharpUnionCase with
    member this.GetPats() =
        if this.HasFields then
            [ SynPat.Paren(
                SynPat.Tuple(
                    false, 
                    [ for f in this.Fields -> f.Name.Named() ],
                    [],
                    range.Zero
                ), 
                range.Zero) 
            ]
        else
            []
        |> SynArgPats.Pats

    member case.LongIdent() : SynPat =
        SynPat.LongIdent(
            longDotId = case.CompiledName.LongIdentWithDots(),
            //propertyKeyword = None,
            extraId = None,
            typarDecls = None,
            argPats = case.GetPats(),
            accessibility = None,
            range = range.Zero
        )