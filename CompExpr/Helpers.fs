module CompExpr.Helpers

open FSharp.Compiler
open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia
open FSharp.Compiler.Text

type Ident with
    static member ofString(s) = Ident(s, Text.range ())

type SynExpr with
    member expr.WrapInParens() =
        SynExpr.Paren(expr, range.Zero, Some range.Zero, range.Zero)

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
            fromMethod = false,
            inLambdaSeq = false,
            args = SynSimplePats.SimplePats([], expr.Range),//dunno about this
            //arrow = Some (range.Zero),
            body = expr,
            parsedData = Some(args,expr),
            range = expr.Range,
            trivia = { ArrowRange = Some (range.Zero) }
        )

