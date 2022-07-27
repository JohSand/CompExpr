module CompExpr.CompileHelpers

open System.Diagnostics
open System.IO
open Fantomas
open FSharp.Compiler
open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia
open FSharp.Compiler.Xml
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols

open CompExpr.Helpers

let tmpRange = Text.range()

let makeType str =
    SynType.LongIdent (LongIdentWithDots ([ Ident.ofString str ], []))

let makeGenericType (typeName) (genericTypes) =
    SynType.App (typeName, Some tmpRange, genericTypes, [], Some tmpRange, false,  tmpRange)

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
        SynPat.Typed(
            SynPat.Named(Ident.ofString (paramName.Replace("@", "")), false, None, tmpRange),
            t,
            tmpRange
        ),
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

        SynType.LongIdent(
            LongIdentWithDots(typeName, [ Text.range.Zero ])
        )

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
    if memberOrFunctionValue.FullName.StartsWith "unitVar" then
        SynPat.Paren(SynPat.Const(SynConst.Unit, tmpRange),tmpRange)
    else
        createSynPat memberOrFunctionValue.FullType memberOrFunctionValue.LogicalName

let createBinding (value: FSharpMemberOrFunctionOrValue) body =
    SynBinding.SynBinding(
        None,
        SynBindingKind.Normal,
        false,
        value.IsMutable,
        [],
        PreXmlDoc.Empty,
        SynValData(None, SynValInfo([], SynArgInfo([], false, None)), None),
        SynPat.Named(Ident.ofString value.LogicalName, value.IsMemberThisValue, None, Text.range()),
        //SynPat.Wild(Text.range.Zero),
        None,
        body,
        Text.range(),
        DebugPointAtBinding.Yes (Text.range()),
        { SynBindingTrivia.LetKeyword = Some (Text.range.Zero); EqualsRange = Some (Text.range.Zero)  }
     )


let createLetDecl bindingName (args: list<list<FSharpMemberOrFunctionOrValue>>) (bindingBody: SynExpr) =
    let range = bindingBody.Range

    let myArgs = 
        if List.isNotEmpty args then
            SynPat.LongIdent(
                LongIdentWithDots.LongIdentWithDots([ Ident(bindingName, range) ], []),
                None, 
                None, 
                None,                
                SynArgPats.Pats(args |> List.collect id |> List.map getArgs),
                None,
                range
              )            
        else
            SynPat.Named(Ident.ofString bindingName, false, None, range)
    SynModuleDecl.Let(
        false,
        [ SynBinding.SynBinding(
              None,
              SynBindingKind.Normal,
              false,
              false,
              [],
              PreXmlDoc.Empty,
              SynValData(None, SynValInfo([], SynArgInfo([], false, None)), None),//here
              myArgs,
              None,
              bindingBody,
              range,
              DebugPointAtBinding.Yes (range),
              { EqualsRange = Some (Text.range()); LetKeyword = Some range }
           )
        
        ],
        Text.range())

let createAnonymousModule members =
    SynModuleOrNamespace(
        longId = [ Ident("Tmp", Text.range ())],
        isRecursive = false,
        kind=SynModuleOrNamespaceKind.AnonModule,
        decls= [ for (name, args, body) in members do createLetDecl name args body ],
        xmlDoc=PreXmlDoc.Empty,
        attribs=[],
        accessibility=None,
        range=Text.range()
    )
let createParsedFileInput members =
    ParsedImplFileInput(
        "tmp.fsx",
        true,
        QualifiedNameOfFile (Ident("Tmp", Text.range ())),
        scopedPragmas = [],
        hashDirectives = [],
        modules = [ createAnonymousModule members ],
        isLastCompiland = (true, true)
    ) |> ParsedInput.ImplFile



let private checker =
    FSharpChecker.Create(keepAssemblyContents = true)

let getTypedParseTree (input) : Async<_> =
    let tmpName = Path.GetTempFileName()
    let currentDir = Directory.GetCurrentDirectory() |> DirectoryInfo
    let deps = currentDir.GetFiles("*.dll") |> Array.map (fun fi -> $"-r:%s{fi.Name}")
    let projectOptions =   
        checker.GetProjectOptionsFromCommandLineArgs(
            Path.ChangeExtension(tmpName, ".fsproj"),
            [| yield "--out:" + Path.ChangeExtension(tmpName, ".dll")
               yield "--flaterrors"
               yield "--targetprofile:netstandard"

               yield Path.ChangeExtension(tmpName, ".fs")
               yield! deps

               yield "--target:exe" |]
        )


    async {
        let input = Text.SourceText.ofString input
        let! results, typedRes = checker.ParseAndCheckFileInProject(Path.ChangeExtension(tmpName, ".fs"), 0, input, projectOptions)

        match typedRes with
        | FSharpCheckFileAnswer.Aborted -> 
            if results.Diagnostics.Length > 0 then
                let diag = System.String.Join(System.Environment.NewLine, results.Diagnostics)
                return Error diag
            else
                return Error("Aborted")
        | FSharpCheckFileAnswer.Succeeded res ->
            for d in res.Diagnostics do 
                printfn $"%s{d.Message}"
            match res.ImplementationFile with
            | None -> return Error $"%A{res.Diagnostics}"
            | Some fc -> return Ok(fc.Declarations)
    }

let writeFormated members =
    async {
        let input = createParsedFileInput members
        let! wat = CodeFormatter.IsValidASTAsync(input)
        return! CodeFormatter.FormatASTAsync(input, "/tmp.fsx", [], None, Fantomas.FormatConfig.FormatConfig.Default)
    }
