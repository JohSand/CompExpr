module CompExpr.Helpers

open FSharp.Compiler
open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia
open FSharp.Compiler.Text

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

