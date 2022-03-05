module CompExpr.Helpers

open System.Diagnostics
open System.IO
open Fantomas
open FSharp.Compiler
open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia
open FSharp.Compiler.Xml
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Text
open FSharp.Compiler.Symbols

type Ident with
    static member ofString(s) = Ident(s, Text.range ())

type SynExpr with
    member expr.WrapInParens() =
        SynExpr.Paren(expr, range.Zero, None, range.Zero)

    member expr.BodyOfLambda(arg) = expr.BodyOfLambda([ arg ])
    /// f expr
    member args.ApplyTo(f) =
        SynExpr.App(
            ExprAtomicFlag.Atomic,
            false, 
            funcExpr = f, 
            argExpr = args, 
            range = Text.range ()
        )

    member expr.BodyOfLambda(args) =
        SynExpr.Lambda(
            false,
            false,
            SynSimplePats.SimplePats([], range.Zero),//dunno about this
            expr,
            Some(args,expr),
            range.Zero,
            SynExprLambdaTrivia.Zero
        )

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
              SynPat.Named(Ident.ofString bindingName, false, None, Text.range()),
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
        longId = [ Ident.ofString "Tmp" ],
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
        QualifiedNameOfFile (Ident.ofString "Tmp"),
        scopedPragmas = [],
        hashDirectives = [],
        modules = [ createAnonymousModule members ],
        isLastCompiland = (true, true)
    ) |> ParsedInput.ImplFile



let private checker =
    FSharpChecker.Create(keepAssemblyContents = true)

let getTypedParseTree (input) : Async<_> =
    let tmpName = Path.GetTempFileName()
    let dep = Path.ChangeExtension(Process.GetCurrentProcess().ProcessName, ".dll")
    //todo ensure we link in all dlls?
    let projectOptions =   
        checker.GetProjectOptionsFromCommandLineArgs(
            Path.ChangeExtension(tmpName, ".fsproj"),
            [| yield "--out:" + Path.ChangeExtension(tmpName, ".dll")
               yield "--flaterrors"
               yield "--targetprofile:netcore"
               yield Path.ChangeExtension(tmpName, ".fs")
               yield "-r:" + "CompExpr.dll"
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