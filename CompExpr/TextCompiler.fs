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
let private resolveNugets input =
    async {
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
                    |> Seq.map (fun (path, _) -> path)
                | _ -> failwith ""
        | _ -> return Seq.empty
    }

let private getTypedParseTree (input) : Async<_> =
    async {

        let! nugets = resolveNugets input

        let tmpName = Path.GetTempFileName()
        let script = Path.ChangeExtension(tmpName, ".fsx")

        let projectOptions =
            checker.GetProjectOptionsFromCommandLineArgs(
                Path.ChangeExtension(tmpName, ".fsproj"),
                [|
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
    | NamedExpression of SynPat * SynExpr

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

let private createLetDecl
    (args: SynPat)
    (bindingBody: SynExpr)
    =
    SynModuleDecl.Let(
        false,
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
                headPat = args
                    ,
                returnInfo = None,
                expr = bindingBody,
                range = range.Zero,
                debugPoint = DebugPointAtBinding.NoneAtLet,
                trivia = {
                    LeadingKeyword = SynLeadingKeyword.Let(range.Zero)
                    InlineKeyword = None
                    EqualsRange = Some(range.Zero)
                }

            )
        ],
        range.Zero
    )


let private writeFormated (fragments: CodeFragment list) =
    async {
        let decls = [
            for fragment in fragments do
                match fragment with
                | Anonymous body -> SynModuleDecl.Expr(body, range.Zero)
                | NamedExpression( args, body) -> createLetDecl args body
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
            //|> ParsedInput.ImplFile
            // Unchecked.defaultof<ParsedImplFileInput>
            |> FCS.Syntax.ParsedInput.ImplFile

        //let! wat = CodeFormatter.IsValidASTAsync(input)
        return! CodeFormatter.FormatASTAsync(input)
    }



let rec private getUntypedParseTree decl = [
    match decl with
    // Represents the declaration of a type
    | FSharpImplementationFileDeclaration.Entity(_entity, decls) ->
        for decl in decls do
            yield! getUntypedParseTree decl

    // Represents the declaration of a member, function or value, including the parameters and body of the member
    | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue(value, args: list<list<FSharpMemberOrFunctionOrValue>>, body: FSharpExpr) ->
        let  IdentPat =
            value.CompiledName.IdentPat(args |> List.collect id)


        yield (NamedExpression(IdentPat, body.ToUntyped()))

    | FSharpImplementationFileDeclaration.InitAction body ->
        // Represents the declaration of a static initialization action
        //[ "anon", [], body.ToUntyped() ]
        yield (Anonymous(body.ToUntyped()))
]

let toLower str =
    async {
        match! getTypedParseTree str with
        | Ok([ decls ]) -> return! decls |> getUntypedParseTree |> writeFormated
        | Error s -> return failwithf $"%s{s}"
        | Ok(l) -> return failwithf "%A" l
    }
