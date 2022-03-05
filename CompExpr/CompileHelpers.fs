module CompExpr.CompileHelpers

open System.Diagnostics
open System.IO
open Fantomas
open FSharp.Compiler
open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia
open FSharp.Compiler.Xml
open FSharp.Compiler.CodeAnalysis

let createLetDecl bindingName bindingBody =
    SynModuleDecl.Let(
        false,
        [ SynBinding.SynBinding(
              None,
              SynBindingKind.Normal,
              false,
              false,
              [],
              PreXmlDoc.Empty,
              SynValData(None, SynValInfo([], SynArgInfo([], false, None)), None),
              SynPat.Named(Ident(bindingName, Text.range ()), false, None, Text.range()),
              //SynPat.Wild(Text.range.Zero),
              None,
              bindingBody,
              Text.range(),
              DebugPointAtBinding.Yes (Text.range()),
              SynBindingTrivia.Zero
           )
        
        ],
        Text.range())

let createAnonymousModule members =
    SynModuleOrNamespace(
        longId = [ Ident("Tmp", Text.range ())],
        isRecursive = false,
        kind=SynModuleOrNamespaceKind.AnonModule,
        decls= [ for (name, body) in members do createLetDecl name body ],
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
               yield "--targetprofile:netcore"
               yield Path.ChangeExtension(tmpName, ".fs")
               yield! deps
               yield "--target:exe" |]
        )


    async {
        let input = Text.SourceText.ofString input
        let! _, typedRes = checker.ParseAndCheckFileInProject(Path.ChangeExtension(tmpName, ".fs"), 0, input, projectOptions)

        match typedRes with
        | FSharpCheckFileAnswer.Aborted -> return Error("Aborted")
        | FSharpCheckFileAnswer.Succeeded res ->
            for d in res.Diagnostics do 
                printfn "%s" d.Message
            match res.ImplementationFile with
            | None -> return Error(sprintf "%A" res.Diagnostics)
            | Some fc -> return Ok(fc.Declarations)
    }

let writeFormated members =
    let input = createParsedFileInput members
    CodeFormatter.FormatASTAsync(input, "/tmp.fsx", [], None, Fantomas.FormatConfig.FormatConfig.Default)

