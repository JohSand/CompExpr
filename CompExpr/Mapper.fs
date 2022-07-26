module CompExpr.Mapper

open CompExpr.Helpers

open FSharp.Compiler
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Symbols.FSharpExprPatterns
open FSharp.Compiler.Syntax

open FSharp.Compiler.Xml

let tmpRange = Text.range()

let makeType str =
    SynType.LongIdent (LongIdentWithDots ([ Ident.ofString str ], []))



let rec toSynType (fsType: FSharpType) =
    if fsType.GenericArguments.Count > 0 then
        let inner = fsType.GenericArguments |> Seq.map toSynType |> Seq.toList
        SynType.App (
            makeType fsType.TypeDefinition.DisplayName, //todo namespace
            Some tmpRange, 
            inner,// generic types 
            [],
            Some tmpRange, 
            false, 
            tmpRange)
    else
        makeType fsType.TypeDefinition.DisplayName

let createTyped (paramName: string) paramType =
    SynPat.Paren(
        SynPat.Typed(
            SynPat.Named(Ident.ofString (paramName.Replace("@", "")), false, None, tmpRange),
            makeType paramType,
            tmpRange
        ),
        tmpRange
    ) 

let getArgsFromType (fullType: FSharpType) (logicalName) =
    if (fullType.HasTypeDefinition) then
        createTyped 
            logicalName
            fullType.TypeDefinition.LogicalName
    elif (fullType.IsTupleType) then
        //todo
        let tupleType =
            SynType.Tuple(
                fullType.IsStructTupleType, 
                [ for _ in fullType.GenericArguments -> false, SynType.Anon(tmpRange) ],
                tmpRange
            )
        SynPat.Paren(
            SynPat.Typed(
                SynPat.Named(Ident.ofString (logicalName.Replace("@", "")), false, None, tmpRange),
                tupleType,
                tmpRange
            ),
            tmpRange
        ) 
    else
        SynPat.Paren(
            SynPat.Named(
                Ident.ofString (logicalName.Replace("@", "")),
                false,
                None,
                tmpRange
            ),
            tmpRange
        ) 

let getArgs (memberOrFunctionValue: FSharpMemberOrFunctionOrValue) =
    if memberOrFunctionValue.FullName = "unitVar" then
        SynPat.Paren(SynPat.Const(SynConst.Unit, tmpRange),tmpRange)
    else
        getArgsFromType memberOrFunctionValue.FullType memberOrFunctionValue.LogicalName

let createBinding name body =
    SynBinding.SynBinding(
        None,
        SynBindingKind.Normal,
        false,
        false,
        [],
        PreXmlDoc.Empty,
        SynValData(None, SynValInfo([], SynArgInfo([], false, None)), None),
        SynPat.Named(Ident.ofString name, false, None, Text.range()),
        //SynPat.Wild(Text.range.Zero),
        None,
        body,
        Text.range(),
        DebugPointAtBinding.Yes (Text.range())
     )
let createIdent(args) =
    let args = args |> List.map(fun (s: string) -> s.Replace("@", "")) |> List.map(Ident.ofString) 
    SynExpr.LongIdent(false, LongIdentWithDots(args, [ tmpRange ]), None, tmpRange)

let createTupled a =
    SynExpr.Tuple(false, a, [], Text.range.Zero)

let rec toUntyped (fsharpExpr: FSharpExpr) : SynExpr =
    match fsharpExpr with
    | Application (expr, types, [ arg ]) -> 
        let arg = toUntyped arg
        arg.ApplyTo((toUntyped expr).WrapInParens())

    | Application (expr, types, args) -> 
        let argsToTheCall = args |> List.map toUntyped |> createTupled
        argsToTheCall.WrapInParens().ApplyTo(toUntyped expr)
    | Lambda (args, expression) ->
        let args = getArgs args    
        let body = toUntyped expression           
        body.BodyOfLambda(args)
    | Call (Some (Call (_,f,_,_,_)), b, c, d, args) -> 
        let argsToTheCall = args |> List.map toUntyped |> createTupled
        //eg Thing.do a, Thing.other a b, Thing.third (a, b), obj.Method(a, b) etc
        let functionCall = createIdent [ f.LogicalName; b.LogicalName ]
        let xkcd = argsToTheCall.WrapInParens().ApplyTo(functionCall)
        xkcd
        //failwith ""
    | Call (Some (Value a), b, c, d, [ f ]) ->
        match toUntyped(f) with
        | SynExpr.Const _ as c -> c.ApplyTo(createIdent [ a.LogicalName; b.LogicalName ])
        | args ->  args.WrapInParens().ApplyTo(createIdent [ a.LogicalName; b.LogicalName ])

    | Call (Some (Value obj), b, c, d, args) -> 
        let argsToTheCall = args |> List.map toUntyped |> createTupled
        //eg Thing.do a, Thing.other a b, Thing.third (a, b), obj.Method(a, b) etc
        let functionCall = createIdent [ obj.LogicalName; b.LogicalName ]
        argsToTheCall.WrapInParens().ApplyTo(functionCall)

    | Call (None, caller, c, d, []) -> 
        createIdent [ caller.LogicalName ]

    | Call (None, caller, c, d, args) when caller.CurriedParameterGroups.Count > 1 -> 
        [ caller.FullName.Split(".") |> List.ofArray |> createIdent
          yield! args |> List.map toUntyped ]                
        |> List.reduce (fun agg curr -> curr.ApplyTo(agg))

    | Call (None, caller, _, _, [ Const (null, _) ]) ->
        let functionCall = caller.FullName.Split(".") |> List.ofArray |> createIdent
        SynExpr.Const(SynConst.Unit, tmpRange).ApplyTo(functionCall)

    | Call (None, caller, c, d, args) ->
        let argsToTheCall = args |> List.map toUntyped |> createTupled
        let functionCall = caller.FullName.Split(".") |> List.ofArray |> createIdent
        argsToTheCall.WrapInParens().ApplyTo(functionCall)

    | Const (c, _) ->
        match c with
        | :? int as i -> 
            SynExpr.Const(SynConst.Int32 i, tmpRange)
        | :? unit -> 
            SynExpr.Const(SynConst.Unit, tmpRange)
        | :? bool as b -> 
            SynExpr.Const(SynConst.Bool b, tmpRange)
        | :? string as s ->
            SynExpr.Const(SynConst.String (s, SynStringKind.Regular,tmpRange), tmpRange)
        | _ -> failwith ""
    | Let ((a, ex1), ex2) -> 
        let body = toUntyped ex2
        let binding = (toUntyped ex1)
        let f = createBinding a.LogicalName binding
        SynExpr.LetOrUse(false, false, [ f ], body, Text.range.Zero)
        //failwith ""
    | NewUnionCase (``type``, case, []) -> 
        let arg = createTupled []
        let ident = LongIdentWithDots([ ``type``.TypeDefinition.DisplayName |> Ident.ofString; case.CompiledName |> Ident.ofString ], [])
        arg.ApplyTo(SynExpr.LongIdent(false, ident, None, tmpRange))

    | NewUnionCase (t, case, expr) -> 
        let arg = expr |> List.map toUntyped |> createTupled
        let ident = LongIdentWithDots([ t.TypeDefinition.DisplayName |> Ident.ofString; case.CompiledName |> Ident.ofString ], [])
        arg.WrapInParens().ApplyTo(SynExpr.LongIdent(false, ident, None, tmpRange))

    | Value value -> 
        value.LogicalName.Replace("@", "") |> Ident.ofString |> SynExpr.Ident
    | TupleGet (b, index, (Value value)) ->
        [ value.LogicalName; $"Item{(index + 1  )}" ] |> createIdent
        //SynExpr.Tuple(false, [  ], [], tmpRange)
    | NewTuple (_, exprs) ->
        let a = exprs |> List.map toUntyped
        SynExpr.Tuple(false, a, [], tmpRange)
    | Coerce (fsType, fsExpr) ->
        //let e = SynExpr.ArrayOrListComputed (false, SynExpr.Const (SynConst.Int32 1, tmpRange), tmpRange)
        let e = toUntyped fsExpr
        SynExpr.Upcast(e, toSynType fsType, tmpRange)
    | TypeLambda (_, expr) -> 
        let bod = toUntyped expr
        SynExpr.LetOrUse(false, false, [  ], bod, Text.range.Zero)
    | Sequential (ex1, ex2) ->
        let dbg = DebugPointAtSequential.SuppressBoth
        let e1 = toUntyped ex1
        let e2 = toUntyped ex2
        SynExpr.Sequential(dbg,false,e1,e2,Text.Range.Zero)
    | _a -> failwith ""

let rec getUntypedParseTree =
    function
    /// Represents the declaration of a type
    | FSharpImplementationFileDeclaration.Entity (entity, decls) ->
        [ for decl in decls do
              yield! getUntypedParseTree decl ]
    /// Represents the declaration of a member, function or value, including the parameters and body of the member
    | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue (value, _, body: FSharpExpr) ->
        //let wat = toUntyped body
        [ value.LogicalName, toUntyped body ]
    /// Represents the declaration of a static initialization action
    | FSharpImplementationFileDeclaration.InitAction _ -> failwith ""

let toLower str =
    async {
        match! CompileHelpers.getTypedParseTree str with
        | Ok ([ decls ]) ->    
            return!
                decls 
                |> getUntypedParseTree
                |> CompileHelpers.writeFormated
        | Error s -> return failwithf "%s" s
        | _ -> return failwith ""
    }