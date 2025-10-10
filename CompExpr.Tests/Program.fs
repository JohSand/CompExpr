open System
open System.Diagnostics
open CompExpr.MapperV2
open Fantomas.FCS.Syntax
open Fantomas.FCS.Text
open Fantomas.FCS.Xml
open Fantomas.FCS.SyntaxTrivia
open Fantomas.Core

type ExprProxy(expr: FSharp.Compiler.Symbols.FSharpExpr) =
    let a = sprintf "%A" expr
    let b = expr.ToUntyped()
    let c = SynModuleDecl.Expr(b, Range.Zero)
    let createAnonymousModule =
        SynModuleOrNamespace(
            longId = [ Ident("Tmp", range.Zero) ],
            isRecursive = false,
            kind = SynModuleOrNamespaceKind.AnonModule,
            decls = [ c ],
            xmlDoc = PreXmlDoc.Empty,
            attribs = [],
            accessibility = None,
            range = range.Zero,
            trivia = {
                SynModuleOrNamespaceTrivia.LeadingKeyword = SynModuleOrNamespaceLeadingKeyword.None
            }
        )

    let input =
        ParsedImplFileInput(
            fileName = "tmp.fsx",
            isScript = true,
            qualifiedNameOfFile = QualifiedNameOfFile(Ident("Tmp", range.Zero)),
            scopedPragmas = [],
            hashDirectives = [],
            contents = [ createAnonymousModule ],
            flags = (true, true),
            trivia = {
                CodeComments = []
                ConditionalDirectives = []
            },
            identifiers = Set.empty
        )
        |> ParsedInput.ImplFile
    let result = CodeFormatter.FormatASTAsync(input) |> Async.RunSynchronously
        
    member val Test = result
    
//[<assembly: DebuggerTypeProxy(typeof<ExprProxy>, Target = typeof<FSharp.Compiler.Symbols.FSharpExpr>)>]
//do()
module Program =
    [<EntryPoint>]
    let main _ = 0
