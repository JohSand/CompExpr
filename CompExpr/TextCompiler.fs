module CompExpr.TextCompiler


open CompExpr.ExprHelpers
open CompExpr.MapperV2
open System
open System.IO
open Fantomas
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

let private createLetDecl bindingName (args: list<list<FSharpMemberOrFunctionOrValue>>) (bindingBody: SynExpr) =
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
        [
            SynBinding.SynBinding(
                None,
                SynBindingKind.Normal,
                false,
                false,
                [],
                PreXmlDoc.Empty,
                SynValData(None, SynValInfo([], SynArgInfo([], false, None)), None), //here
                myArgs,
                None,
                bindingBody,
                range,
                DebugPointAtBinding.Yes(range),
                {
                    EqualsRange = Some(Text.range ())
                    LetKeyword = Some range
                }
            )

        ],
        Text.range ()
    )

let private createAnonymousModule members =
    SynModuleOrNamespace(
        longId = [ Ident("Tmp", Text.range ()) ],
        isRecursive = false,
        kind = SynModuleOrNamespaceKind.AnonModule,
        decls = [ for (name, args, body) in members -> createLetDecl name args body ],
        xmlDoc = PreXmlDoc.Empty,
        attribs = [],
        accessibility = None,
        range = Text.range ()
    )

let private writeFormated members =
    async {
        let input =
            ParsedImplFileInput(
                "tmp.fsx",
                true,
                QualifiedNameOfFile(Ident("Tmp", Text.range ())),
                scopedPragmas = [],
                hashDirectives = [],
                modules = [ createAnonymousModule members ],
                isLastCompiland = (true, true)
            )
            |> ParsedInput.ImplFile

        let! wat = CodeFormatter.IsValidASTAsync(input)
        return! CodeFormatter.FormatASTAsync(input, "/tmp.fsx", [], None, Fantomas.FormatConfig.FormatConfig.Default)
    }

let rec private getUntypedParseTree =
    function
    // Represents the declaration of a type
    | FSharpImplementationFileDeclaration.Entity(entity, decls) -> [
        for decl in decls do
            yield! getUntypedParseTree decl
      ]
    // Represents the declaration of a member, function or value, including the parameters and body of the member
    | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue(value, args: list<list<_>>, body: FSharpExpr) ->
        //let wat = toUntyped body
        [ value.LogicalName, args,  body.ToUntyped() ]
    // Represents the declaration of a static initialization action
    | FSharpImplementationFileDeclaration.InitAction body -> [ "anon", [], body.ToUntyped() ]

let toLower str =
    async {
        match! getTypedParseTree str with
        | Ok([ decls ]) -> return! decls |> getUntypedParseTree |> writeFormated
        | Error s -> return failwithf $"%s{s}"
        | _ -> return failwith ""
    }
