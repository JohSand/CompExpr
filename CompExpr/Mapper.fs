module CompExpr.Mapper

open CompExpr.ExprHelpers

open FSharp.Compiler
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Symbols.FSharpExprPatterns
open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia
open FSharp.Compiler.Xml

open System

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
    | Call(Some(Call(_, f, _, _, _)), b, c, d, args) ->
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
        let argsToTheCall = args |> List.map toUntyped |> createTupled
        //eg Thing.do a, Thing.other a b, Thing.third (a, b), obj.Method(a, b) etc
        let functionCall = createIdent [ obj.LogicalName; b.LogicalName ]
        argsToTheCall.WrapInParens().ApplyTo(functionCall)

    | Call(None, caller, c, d, []) -> createIdent [ caller.LogicalName ]

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
            [ f ],
            body,
            Option.defaultValue Text.range.Zero inKeyword,
            { InKeyword = inKeyword }
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
        let typeName =
            expr.TypeDefinition.FullName
            |> fun s -> s.Split(".")
            |> Array.map Ident.ofString
            |> List.ofArray

        SynExpr.New(
            false,
            SynType.LongIdent(LongIdentWithDots(typeName, [ Text.range.Zero ])),
            SynExpr.Const(SynConst.Unit, Text.range.Zero),
            tmpRange
        )
    | NewObject(f, types, exprs) ->

        let pats = SynArgPats.Pats([ getArgs f ])

        let argsToCtor =
            if List.isEmpty exprs then
                SynExpr.Const(SynConst.Unit, Text.range.Zero)
            else
                (exprs |> List.map toUntyped |> createTupled).WrapInParens()
        //argsToCtor.WrapInParens().ApplyTo(toUntyped expr)
        let typeName = toSynTypeFromClrType (f.ApparentEnclosingEntity).DisplayName types
        SynExpr.New(false, typeName, argsToCtor, tmpRange)
    | _a ->
        raise
        <| NotImplementedException($"Mapping to untyped tree not implemented for {_a}")
