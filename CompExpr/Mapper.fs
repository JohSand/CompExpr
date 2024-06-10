module CompExpr.Mapper

open CompExpr.ExprHelpers

open FSharp.Compiler
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Symbols.FSharpExprPatterns
open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia
open FSharp.Compiler.Text

open System

let getMatchName fsharpExpr =
    match fsharpExpr with
    | IfThenElse(UnionCaseTest(Value expr, _, _), _, _) -> expr.CompiledName
    | _ -> "failed to resolve name"

let getBindName fsharpExpr =
    match fsharpExpr with
    | Let((_, UnionCaseGet(expr, typ, case, field), _), _) -> field.Name
    | _ -> "failed to get name of binding"

let getBoundName fsharpExpr =
    match fsharpExpr with
    | Let((v, _, _), _) -> v.CompiledName
    | _ -> "failed to get name of binding"


let getDecisionTreePat fsharpExpr =
    match fsharpExpr with
    | UnionCaseTest(Value expr, typ, case) ->
        let pats =
            if case.HasFields then
                [
                    SynPat.Paren(
                        SynPat.Tuple(
                            false,
                            [
                                for f in case.Fields do
                                    SynPat.Named(Ident.ofString f.Name, false, None, range.Zero)
                            ],
                            range.Zero
                        ),
                        range.Zero
                    )

                ]
            else
                []

        SynPat.LongIdent(
            longDotId = LongIdentWithDots(id = [ Ident(case.CompiledName, range.Zero) ], dotRanges = []),
            propertyKeyword = None,
            extraId = None,
            typarDecls = None,
            argPats = SynArgPats.Pats(pats),
            accessibility = None,
            range = range.Zero
        )
    | _ -> failwith ""

let rec toUntyped (fsharpExpr: FSharpExpr) : SynExpr =
    let tmpRange = fsharpExpr.Range

    match fsharpExpr with
    | Application(expr, types, [ arg ]) ->
        let arg = toUntyped arg
        arg.ApplyTo((toUntyped expr).WrapInParens())

    | Application(expr, types, args) ->
        let argsToTheCall = args |> List.map toUntyped |> createTupled
        argsToTheCall.WrapInParens().ApplyTo(toUntyped expr)
    | Lambda(args, expression) ->
        let args = getArgs args
        let body = toUntyped expression
        body.BodyOfLambda(args)
    | Call(Some(Call(maybeE, f, _typs, _, thisArgs)), b, c, d, args) ->
        if f.IsMember then
            let argsToTheCall = args |> List.map toUntyped |> createTupled
            //eg Thing.do a, Thing.other a b, Thing.third (a, b), obj.Method(a, b) etc
            let functionCall = 
                createIdent [ 
                    match maybeE with
                    | Some (Value a) -> 
                        a.LogicalName
                    | _ ->
                        ()
                    f.LogicalName.Replace("get_", ""); 
                    b.LogicalName 
                ]
            let xkcd = argsToTheCall.WrapInParens().ApplyTo(functionCall)
            xkcd

        else
            let argsToTheCall = args |> List.map toUntyped |> createTupled
            //eg Thing.do a, Thing.other a b, Thing.third (a, b), obj.Method(a, b) etc
            let functionCall = createIdent [ f.LogicalName; b.LogicalName ]
            let xkcd = argsToTheCall.WrapInParens().ApplyTo(functionCall)
            xkcd
    //failwith ""
    | Call(Some(Value a), b, c, d, [ f ]) ->
        match toUntyped (f) with
        | SynExpr.Const _ as c -> c.ApplyTo(createIdent [ a.LogicalName; b.LogicalName ])
        | args -> args.WrapInParens().ApplyTo(createIdent [ a.LogicalName; b.LogicalName ])

    | Call(Some(Value obj), b, c, d, args) ->
        let argsToTheCall = 
            args 
            |> List.map toUntyped
            |> List.map (fun s -> 
                if false then
                    s.WrapInParens()
                else 
                    s
            )
            |> createTupled
        //eg Thing.do a, Thing.other a b, Thing.third (a, b), obj.Method(a, b) etc
        let functionCall = createIdent [ obj.LogicalName; b.LogicalName ]
        argsToTheCall.WrapInParens().ApplyTo(functionCall)

    | Call(None, caller, c, [], []) -> 
        if caller.ApparentEnclosingEntity.IsFSharpModule then
            //static call, no args
            if caller.ApparentEnclosingEntity.HasAttribute<AutoOpenAttribute>() then
                //caller.ApparentEnclosingEntity
                createIdent [ caller.CompiledName  ]
            else
                createIdent [ 
                    caller.ApparentEnclosingEntity.CompiledName
                    caller.CompiledName  
                ]
        else
            if caller.IsPropertyGetterMethod then
            //static call probably unit args?
                createIdent [ 
                    caller.ApparentEnclosingEntity.CompiledName
                    caller.CompiledName.Replace("get_", "")  
                ]
            else
                let functionCall = createIdent [ 
                    caller.ApparentEnclosingEntity.CompiledName
                    caller.CompiledName  
                ]
                SynExpr.Const(SynConst.Unit, tmpRange).ApplyTo(functionCall)


    | Call(None, caller, c, genericArgs, []) -> 
        //static call
        let functionCall = 
            createIdent [ 
                caller.ApparentEnclosingEntity.CompiledName
                caller.CompiledName 
            ]
        let typeArgs = 
            genericArgs
            |> List.map getSynType
        SynExpr.Const(SynConst.Unit, tmpRange).ApplyTo(functionCall.WithTypeArgs(typeArgs))

    //operators
    | Call(None, caller, c, d, args) when caller.CompiledName.StartsWith("op_") ->
        [ createIdent [ caller.LogicalName ]; yield! args |> List.map toUntyped ]
        |> List.reduce (fun agg curr -> curr.ApplyToInfix(agg))
    //fsharp function calls
    | Call(None, caller, c, d, args) when caller.CurriedParameterGroups.Count > 1 ->
        [
            caller.FullName.Split(".") |> List.ofArray |> createIdent
            yield! args |> List.map toUntyped
        ]
        |> List.reduce (fun agg curr -> curr.ApplyTo(agg))

    | Call(None, caller, _, _, [ Const(null, _) ]) ->
        let functionCall = caller.FullName.Split(".") |> List.ofArray |> createIdent
        SynExpr.Const(SynConst.Unit, tmpRange).ApplyTo(functionCall)

    | Call(None, caller, c, d, args) ->
        let argsToTheCall = args |> List.map toUntyped |> createTupled
        let functionCall = caller.FullName.Split(".") |> List.ofArray |> createIdent
        argsToTheCall.WrapInParens().ApplyTo(functionCall)

    | Const(c, _) ->
        match c with
        | :? int as i -> SynExpr.Const(SynConst.Int32 i, tmpRange)
        | :? unit -> SynExpr.Const(SynConst.Unit, tmpRange)
        | :? bool as b -> SynExpr.Const(SynConst.Bool b, tmpRange)
        | :? string as s -> SynExpr.Const(SynConst.String(s, SynStringKind.Regular, tmpRange), tmpRange)
        | _ -> failwith ""
    | Let((a, ex1, dbg: DebugPointAtBinding), ex2) ->
        let body = toUntyped ex2
        let binding = (toUntyped ex1)
        let f = createBinding a binding

        let inKeyword =
            match dbg with
            | DebugPointAtBinding.Yes a -> Some a
            | _ -> None

        SynExpr.LetOrUse(
            false,
            false,
            bindings = [ f ],
            body = body,
            range = Option.defaultValue Text.range.Zero inKeyword,
            trivia = { InKeyword = inKeyword }
        )
    //failwith ""
    | NewUnionCase(``type``, case, []) ->
        let arg = createTupled []

        let ident =
            LongIdentWithDots(
                [
                    ``type``.TypeDefinition.DisplayName |> Ident.ofString
                    case.CompiledName |> Ident.ofString
                ],
                []
            )

        arg.ApplyTo(SynExpr.LongIdent(false, ident, None, tmpRange))

    | NewUnionCase(t, case, expr) ->
        let arg = expr |> List.map toUntyped |> createTupled

        let ident =
            LongIdentWithDots(
                [
                    t.TypeDefinition.DisplayName |> Ident.ofString
                    case.CompiledName |> Ident.ofString
                ],
                []
            )

        arg.WrapInParens().ApplyTo(SynExpr.LongIdent(false, ident, None, tmpRange))

    | Value value -> value.LogicalName.Replace("@", "") |> Ident.ofString |> SynExpr.Ident
    | TupleGet(b, index, (Value value)) -> [ value.LogicalName; $"Item{(index + 1)}" ] |> createIdent
    //SynExpr.Tuple(false, [  ], [], tmpRange)
    | NewTuple(_, exprs) ->
        let a = exprs |> List.map toUntyped
        SynExpr.Tuple(false, a, [], tmpRange)
    | Coerce(fsType, fsExpr) ->
        //let e = SynExpr.ArrayOrListComputed (false, SynExpr.Const (SynConst.Int32 1, tmpRange), tmpRange)
        let e = toUntyped fsExpr
        SynExpr.Upcast(e, toSynType fsType, tmpRange)
    | TypeLambda(_, expr) ->
        let bod = toUntyped expr
        SynExpr.LetOrUse(false, false, [], bod, Text.range.Zero, { InKeyword = Some Text.range.Zero })
    | Sequential(ex1, ex2) ->
        let dbg = DebugPointAtSequential.SuppressBoth
        let e1 = toUntyped ex1
        let e2 = toUntyped ex2
        SynExpr.Sequential(dbg, false, e1, e2, Text.Range.Zero)
    | ValueSet(value, expr) ->
        let binding = (toUntyped expr)
        let f = createBinding value binding

        SynExpr.LongIdentSet(
            LongIdentWithDots([ Ident.ofString value.LogicalName ], [ Text.range.Zero ]),
            binding,
            Text.Range.Zero
        )
    | DefaultValue expr ->
        if expr.TypeDefinition.IsValueType then
            let name = expr.TypeDefinition.FullName.Replace("`1", "")
            let typeName = toSynTypeFromClrType name [ yield! expr.GenericArguments ]
            SynExpr.New(false, typeName, SynExpr.Const(SynConst.Unit, range.Zero), range.Zero)
        else
            SynExpr.Null(range.Zero)

    | NewObject(f, types, exprs) ->
        let argsToCtor =
            if List.isEmpty exprs then
                SynExpr.Const(SynConst.Unit, Text.range.Zero)
            else
                (exprs |> List.map toUntyped |> createTupled).WrapInParens()
        //argsToCtor.WrapInParens().ApplyTo(toUntyped expr)
        let fullName = f.ApparentEnclosingEntity.FullName.Replace("`1", "")
        let typeName = toSynTypeFromClrType fullName types
        SynExpr.New(false, typeName, argsToCtor, range.Zero)
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
            toUntyped ifExpr,
            toUntyped thenExpr,
            Some(toUntyped elseExpr),
            debugPoint,
            isFromErrorRecovery = false,
            range = range.Zero,
            trivia = trivia
        )

    | Quote(expr) ->
        let inner = toUntyped expr
        SynExpr.Quote(inner, false, inner, false, range.Zero)
    | NewRecord(t, exprs) ->
        let records = [
            for (m, r) in exprs |> Seq.zip t.TypeDefinition.FSharpFields do
                let typeName =
                    m.Name |> (fun s -> s.Split(".")) |> Array.map Ident.ofString |> List.ofArray

                let inner = toUntyped r

                SynExprRecordField.SynExprRecordField(
                    fieldName = RecordFieldName(LongIdentWithDots(typeName, []), false),
                    equalsRange = None,
                    expr = Some inner,
                    blockSeparator = None
                )
        ]

        SynExpr.Record(None, None, records, range.Zero)
    | UnionCaseGet(expr, typ, case, field) ->

        //toUntyped expr
        SynExpr.Ident(Ident.ofString field.Name)

    | DecisionTree(ifElse, nodes) ->
        let clauses = parseDecisionTree ifElse nodes

        SynExpr.Match(
            range.Zero,
            DebugPointAtBinding.NoneAtInvisible,
            SynExpr.Ident(Ident(getMatchName ifElse, range.Zero)),
            range.Zero,
            clauses,
            range.Zero
        )
    | a ->
        let expr = sprintf "%A." a

        SynExpr.App(
            flag = ExprAtomicFlag.NonAtomic,
            isInfix = false,
            funcExpr =
                SynExpr.App(
                    flag = ExprAtomicFlag.NonAtomic,
                    isInfix = false,
                    funcExpr = SynExpr.Ident(Ident("invalidArg", range.Zero)),
                    argExpr =
                        SynExpr.Const(
                            SynConst.String(
                                text = "fsharpExpr",
                                synStringKind = SynStringKind.Regular,
                                range = range.Zero
                            ),
                            range.Zero
                        ),
                    range = range.Zero
                ),
            argExpr =
                SynExpr.Const(
                    SynConst.String(text = expr, synStringKind = SynStringKind.Regular, range = range.Zero),
                    range = range.Zero
                ),
            range = range.Zero
        )

and parseDecisionTree (fsharpExpr: FSharpExpr) (result: (_ * FSharpExpr) list) = [
    match fsharpExpr with
    | IfThenElse(test, expr, Call(_)) ->
        let pat = getDecisionTreePat test

        SynMatchClause(
            pat = pat,
            whenExpr = None, //todo
            resultExpr = toUntyped (expr),
            range = range.Zero,
            debugPoint = DebugPointAtTarget.No,
            trivia = {
                ArrowRange = Some(range.Zero)
                BarRange = Some(range.Zero)
            }
        )

    | IfThenElse(test, DecisionTreeSuccess(case1, _), DecisionTreeSuccess(case2, _)) ->
        let (_, expr1) = result[case1]
        let pat = getDecisionTreePat test

        SynMatchClause(
            pat = pat,
            whenExpr = None, //todo
            resultExpr = toUntyped (expr1),
            range = range.Zero,
            debugPoint = DebugPointAtTarget.No,
            trivia = {
                ArrowRange = Some(range.Zero)
                BarRange = Some(range.Zero)
            }
        )

        let (_, expr2) = result[case2]

        let finalClause =
            SynMatchClause(
                pat = SynPat.Wild(range.Zero),
                whenExpr = None, //todo
                resultExpr = toUntyped (expr2),
                range = range.Zero,
                debugPoint = DebugPointAtTarget.No,
                trivia = {
                    ArrowRange = Some(range.Zero)
                    BarRange = Some(range.Zero)
                }
            )

        finalClause
    | IfThenElse(test, DecisionTreeSuccess(case, _), rest) ->
        let (_, expr) = result[case]
        let pat = getDecisionTreePat test

        SynMatchClause(
            pat = pat,
            whenExpr = None, //todo
            resultExpr = toUntyped (expr),
            range = range.Zero,
            debugPoint = DebugPointAtTarget.No,
            trivia = {
                ArrowRange = Some(range.Zero)
                BarRange = Some(range.Zero)
            }
        )

        yield! parseDecisionTree rest result

    | IfThenElse(test,
                 IfThenElse(ifExpr, DecisionTreeSuccess(case1, _), DecisionTreeSuccess(case2, _)),
                 DecisionTreeSuccess(case3, _)) ->
        let pat = getDecisionTreePat test
        let whenExpr = toUntyped ifExpr
        let (_, thenExpr) = result[case1]
        let (_, elseExpr) = result[case2]

        //let inner =
        //    SynExpr.IfThenElse(
        //        whenExpr,
        //        toUntyped thenExpr,
        //        Some(toUntyped elseExpr),
        //        debugPoint,
        //        isFromErrorRecovery = false,
        //        range = range.Zero,
        //        trivia = trivia
        //    )
        //construct the result, since the name used in the result is not let-bound
        let boundValue = getBindName ifExpr
        let boundName = getBoundName ifExpr
        let resultExpr = toUntyped thenExpr
        let letResult =
            SynExpr.LetOrUse(
                false,
                false,
                [
                    createLetBinding boundValue boundName
                ],
                resultExpr,
                range.Zero,
                { InKeyword = Some(range.Zero) }
            
            )

        SynMatchClause(
            pat = pat,
            whenExpr = Some(whenExpr), //todo
            resultExpr = letResult,
            range = range.Zero,
            debugPoint = DebugPointAtTarget.No,
            trivia = {
                ArrowRange = Some(range.Zero)
                BarRange = Some(range.Zero)
            }
        )

        yield! parseDecisionTree elseExpr result

        let (_, expr2) = result[case3]

        let finalClause =
            SynMatchClause(
                pat = SynPat.Wild(range.Zero),
                whenExpr = None, //todo
                resultExpr = toUntyped (expr2),
                range = range.Zero,
                debugPoint = DebugPointAtTarget.No,
                trivia = {
                    ArrowRange = Some(range.Zero)
                    BarRange = Some(range.Zero)
                }
            )

        finalClause

    | IfThenElse(test, (IfThenElse(_, _, _) as eif), rest) ->
        let (_, expr) = result[1]
        let pat = getDecisionTreePat test

        SynMatchClause(
            pat = pat,
            whenExpr = None, //todo
            resultExpr = toUntyped (expr),
            range = range.Zero,
            debugPoint = DebugPointAtTarget.No,
            trivia = {
                ArrowRange = Some(range.Zero)
                BarRange = Some(range.Zero)
            }
        )

        yield! parseDecisionTree rest result


    | _ -> failwith "unknown decision-tree option"
]
