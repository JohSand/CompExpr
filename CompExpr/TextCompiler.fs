module CompExpr.TextCompiler


open CompExpr.MapperV2
open System
open System.IO

open FSharp.Compiler
open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia
open FSharp.Compiler.Xml
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Text

let private checker = FSharpChecker.Create(keepAssemblyContents = true)

// Based on https://queil.net/2021/06/embedding-fsharp-compiler-nuget-references/
let private resolveNugets input = task {
    match! checker.GetProjectOptionsFromScript($"%s{Path.GetTempFileName()}.fsx", SourceText.ofString input) with
    | projOptions, [] ->
        let! projResults = checker.ParseAndCheckProject(projOptions)

        return
            match projResults.HasCriticalErrors with
            | false ->
                projResults.DependencyFiles
                |> Seq.choose (function
                    | path when path.EndsWith(".dll") -> Some path
                    | _ -> None)
                |> Seq.groupBy id
                |> Seq.map (fun (path, _) -> path),
                projResults.DependencyFiles
                |> Seq.choose (function
                    | path when path.EndsWith(".fsx") && (not (path.Contains("Local\Temp"))) -> Some path
                    | _ -> None)
                |> Seq.groupBy id
                |> Seq.map (fun (path, _) -> path)
            | true ->
                let errs = projResults.Diagnostics |> Array.map (_.Message)
                let msg = String.Join(Environment.NewLine, value = errs)
                failwith ("Failed to parse and  check project:" + msg)
    | projOptions, diags ->

        let errs = diags |> List.map (_.Message) |> List.toArray
        let msg = String.Join(Environment.NewLine, value = errs)
        failwith ("Failed to resolve project options:" + msg)
        return (Seq.empty, Seq.empty)
}

let private getTypedParseTree (input) = task {

    let! (nugets, loadedScripts) = resolveNugets input
    let tmpName = Path.GetTempFileName()
    let script = Path.ChangeExtension(tmpName, ".fsx")

    let projectOptions =
        checker.GetProjectOptionsFromCommandLineArgs(
            Path.ChangeExtension(tmpName, ".fsproj"),
            [|
                "-a"
                for path in loadedScripts do
                    $"{path}"
                "--out:" + Path.ChangeExtension(tmpName, ".dll")
                "--flaterrors"
                "--targetprofile:netstandard"
                script
                for path in nugets do
                    $"-r:{path}"

                for fi in DirectoryInfo(Directory.GetCurrentDirectory()).GetFiles("*.dll") do
                    $"-r:%s{fi.Name}"
                //yield $"--compilertool:%s{currentDir.FullName}"
                "--debug:full"
                "--target:exe"
            |]
        )

    let! results, typedRes =
        checker.ParseAndCheckFileInProject(script, 0, Text.SourceText.ofString input, projectOptions)

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

open Fantomas
open Fantomas.Core
open Fantomas.FCS.Syntax
open Fantomas.FCS.SyntaxTrivia
open Fantomas.FCS.Text
open Fantomas.FCS.Xml

[<NoComparison; NoEquality>]
type CodeFragment =
    | Anonymous of SynExpr
    | NamedExpression of SynPat * SynExpr * bool * FSharp.Compiler.Text.range

let private createAnonymousModule decls =
    SynModuleOrNamespace(
        longId = [ Ident("Tmp", range.Zero) ],
        isRecursive = false,
        kind = SynModuleOrNamespaceKind.AnonModule,
        decls = decls,
        xmlDoc = PreXmlDoc.Empty,
        attribs = [],
        accessibility = None,
        range = range.Zero,
        trivia = {
            SynModuleOrNamespaceTrivia.LeadingKeyword = SynModuleOrNamespaceLeadingKeyword.None
        }
    )

let private createLetDecl (args: SynPat) (bindingBody: SynExpr) isRec =
    SynModuleDecl.Let(
        isRec,
        [
            SynBinding.SynBinding(
                accessibility = None,
                kind = SynBindingKind.Normal,
                isInline = false,
                isMutable = false,
                attributes = [],
                xmlDoc = PreXmlDoc.Empty,
                valData =
                    SynValData.SynValData(
                        memberFlags = None,
                        valInfo =
                            SynValInfo.SynValInfo(
                                curriedArgInfos = [ [] ],
                                returnInfo = SynArgInfo.SynArgInfo(attributes = [], optional = false, ident = None)
                            ),
                        thisIdOpt = None
                    ),
                headPat = args,
                returnInfo = None,
                expr = bindingBody,
                range = range.Zero,
                debugPoint = DebugPointAtBinding.NoneAtLet,
                trivia = {
                    LeadingKeyword =
                        if isRec then
                            SynLeadingKeyword.LetRec(range.Zero, range.Zero)
                        else
                            SynLeadingKeyword.Let(Range.Zero)
                    InlineKeyword = None
                    EqualsRange = Some(range.Zero)
                }

            )
        ],
        range.Zero
    )


let private writeFormated (fragments: CodeFragment list) = task {
    let decls = [
        for fragment in fragments do
            match fragment with
            | Anonymous body -> SynModuleDecl.Expr(body, range.Zero)
            | NamedExpression(args, body, isRec, _) -> createLetDecl args body isRec
    ]

    let anonModule = createAnonymousModule decls

    let input =
        ParsedImplFileInput(
            fileName = "tmp.fsx",
            isScript = true,
            qualifiedNameOfFile = QualifiedNameOfFile(Ident("Tmp", range.Zero)),
            scopedPragmas = [],
            hashDirectives = [],
            contents = [ anonModule ],
            flags = (true, true),
            trivia = {
                CodeComments = []
                ConditionalDirectives = []
            },
            identifiers = Set.empty
        )
        |> FCS.Syntax.ParsedInput.ImplFile

    //let! wat = CodeFormatter.IsValidASTAsync(input)
    let! formatted = CodeFormatter.FormatASTAsync(input)

    let firstStatement =
        [
            for fragment in fragments do
                match fragment with
                | Anonymous body -> None
                | NamedExpression(_, _, _, range) -> Some(range)
        ]
        |> List.tryPick id

    return formatted, firstStatement
}



let rec private getUntypedParseTree (implText: string) decl = [
    match decl with
    // Represents the declaration of a type
    | FSharpImplementationFileDeclaration.Entity(_entity: FSharpEntity, decls) ->
        for decl in decls do
            yield! getUntypedParseTree implText decl

    // Represents the declaration of a member, function or value, including the parameters and body of the member
    | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue(value,
                                                                  args: list<list<FSharpMemberOrFunctionOrValue>>,
                                                                  body: FSharpExpr) ->
        let name = value.CompiledName
        //so ugly.
        //it seems we wont know if the member, function or value is recursive from the typed AST.
        //but we can find it in the impleText.
        let isRec = implText.Contains($"let rec %s{name}")
        let IdentPat = name.IdentPat(args |> List.collect id)

        yield (NamedExpression(IdentPat, body.ToUntyped(), isRec, value.DeclarationLocation))

    | FSharpImplementationFileDeclaration.InitAction body ->
        // Represents the declaration of a static initialization action
        //[ "anon", [], body.ToUntyped() ]
        test body
        yield (Anonymous(body.ToUntyped()))
]

let dump str = task {
    match! getTypedParseTree str with
    | Ok([ decls ]) ->
        
        return sprintf "%A" decls
    | Error s -> return failwithf $"%s{s}"
    | Ok(l) -> return failwithf "%A" l
}

let toLower str = task {
    match! getTypedParseTree str with
    | Ok([ decls ]) ->
        let! (output, _startRange) = decls |> getUntypedParseTree str |> writeFormated
        return output
    | Error s -> return failwithf $"%s{s}"
    | Ok(l) -> return failwithf "%A" l
}

let toLowerStart str = task {
    match! getTypedParseTree str with
    | Ok([ decls ]) -> return! decls |> getUntypedParseTree str |> writeFormated
    | Error s -> return failwithf $"%s{s}"
    | Ok(l) -> return failwithf "%A" l
}
