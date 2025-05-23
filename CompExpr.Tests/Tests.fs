module Tests

open System
open System.Threading
open System.Threading.Tasks
open System.Threading.Channels
open Xunit
open CompExpr
open Swensen.Unquote.Extensions
open Microsoft.FSharp.Quotations

type Code =
    static member toText([<ReflectedDefinition>] a: Expr<_>) = a.Decompile()

[<Fact>]
let ``Test static creator method`` () = task {
    let! result = TextCompiler.toLower "System.Threading.Channels.Channel.CreateUnbounded<int>()"
    let expected = $"Channel.CreateUnbounded<int>(){Environment.NewLine}"
    do Assert.Equal(expected, result)
    let expected = "Channel.CreateUnbounded<int>()\r\n"
    do Assert.Equal(expected, result, ignoreLineEndingDifferences = true)
}


[<Fact>]
let ``Test csharp property accessor`` () = task {
    let fsharp =
        "\
let a () = task {
    let chan = System.Threading.Channels.Channel.CreateUnbounded<int>()       
    let! reader = chan.Reader.ReadAsync()
    chan.Writer.Complete()
}        
            "

    let! result = TextCompiler.toLower fsharp

    let expected =
        "\
let a () =
    (fun (builder: TaskBuilder) ->
        builder.Run(
            builder.Delay(fun () ->
                let chan = Channel.CreateUnbounded<int>() in

                builder.Bind(
                    chan.Reader.ReadAsync(new System.Threading.CancellationToken()),
                    fun (_arg1: int) ->
                        let reader = _arg1
                        chan.Writer.Complete(null)
                        builder.Zero()
                ))
        ))
        task
"

    do Assert.Equal(expected, result, ignoreLineEndingDifferences = true)
}


[<Fact>]
let ``Test variable let`` () = task {
    let expr =
        <@@
            let a = 1
            ()
        @@>

    let fsharp = expr.Decompile()
    let! result = TextCompiler.toLower fsharp
    let expected = "let a = 1 in ()\r\n"
    do Assert.Equal(expected, result, ignoreLineEndingDifferences = true)
}

[<Fact>]
let ``Test tupled method call`` () = task {
    let expr =
        <@@
            System.Threading.CancellationTokenSource.CreateLinkedTokenSource(
                System.Threading.CancellationToken.None,
                System.Threading.CancellationToken.None
            )
        @@>

    let fsharp = expr.Decompile()

    let! result =
        TextCompiler.toLower
            "System.Threading.CancellationTokenSource.CreateLinkedTokenSource(System.Threading.CancellationToken.None, System.Threading.CancellationToken.None)"

    let expected =
        $"CancellationTokenSource.CreateLinkedTokenSource(CancellationToken.None, CancellationToken.None){Environment.NewLine}"

    do Assert.Equal(expected, result)
}

[<Fact>]
let ``Test plus operator`` () = task {
    let fsharp = Code.toText (1 + 2 + 3)
    let! result = TextCompiler.toLower fsharp
    let expected = "1 + 2 + 3\r\n"
    do Assert.Equal(expected, result, ignoreLineEndingDifferences = true)
}

[<Fact>]
let ``Test prefix operator`` () = task {
    let fsharp = Code.toText (~~~ 2)

    let! result = TextCompiler.toLower fsharp
    let expected = "~~~ 2\r\n"
    do Assert.Equal(expected, result, ignoreLineEndingDifferences = true)
}

[<Fact>]
let ``Test simple lambda`` () = task {
    let fsharp = Code.toText (fun () -> ())

    let! result = TextCompiler.toLower fsharp
    let expected = "fun () -> ()\r\n"
    do Assert.Equal(expected, result, ignoreLineEndingDifferences = true)
}

[<Fact>]
let ``Test simple lambda with typed argument`` () = task {
    let fsharp = Code.toText (fun (a: int) -> a)

    let! result = TextCompiler.toLower fsharp
    let expected = "let patternInput1 a = a\r\n"
    do Assert.Equal(expected, result, ignoreLineEndingDifferences = true)
}

[<Fact>]
let ``Test simple lambda with two typed arguments`` () = task {
    let fsharp = Code.toText (fun (a: int) (b: int) -> a + b)

    let! result = TextCompiler.toLower fsharp
    let expected = "fun (x: int) -> fun (y: int) -> x + y\r\n"
    do Assert.Equal(expected, result, ignoreLineEndingDifferences = true)
}

[<Fact>]
let ``Test single argument let`` () = task {
    let fsharp = "let f a = 1"
    let! result = TextCompiler.toLower fsharp
    let expected = "let f a = 1\r\n"
    do Assert.Equal(expected, result, ignoreLineEndingDifferences = true)
}

[<Fact>]
let ``Test single argument unit let`` () = task {
    let fsharp = "let f () = 1"
    let! result = TextCompiler.toLower fsharp
    let expected = "let f () = 1\r\n"
    do Assert.Equal(expected, result, ignoreLineEndingDifferences = true)
}

[<Fact>]
let ``Test single typed argument let`` () = task {
    let fsharp = "let f (a: int) = a"
    let! result = TextCompiler.toLower fsharp
    let expected = "let f (a: int) = a\r\n"
    do Assert.Equal(expected, result, ignoreLineEndingDifferences = true)
}

[<Fact>]
let ``Test multi typed argument let`` () = task {
    let fsharp = "let f (a: int) (b: int) = a"
    let! result = TextCompiler.toLower fsharp
    let expected = "let f (a: int) (b: int) = a\r\n"
    do Assert.Equal(expected, result, ignoreLineEndingDifferences = true)
}

[<Fact>]
let ``Test single typed generic argument let`` () = task {
    let fsharp = "let f (a: System.Collections.Generic.List<_>) (b: int) = a"
    let! result = TextCompiler.toLower fsharp
    let expected = "let f (a: List<'a>) (b: int) = a\r\n"
    do Assert.Equal(expected, result, ignoreLineEndingDifferences = true)
}

[<Fact>]
let ``Test calling a curried function`` () = task {
    let fsharp = Code.toText (List.map id [])

    let! result = TextCompiler.toLower fsharp

    let expected =
        "Microsoft.FSharp.Collections.List.map fun (x: obj) -> id (x) List.Empty\r\n"

    do Assert.Equal(expected, result, ignoreLineEndingDifferences = true)
}

[<Fact>]
let ``Test lambda end parens`` () = task {
    let fsharp = "id (fun () -> ())"
    let! result = TextCompiler.toLower fsharp
    let expected = "id (fun () -> ())\r\n"
    do Assert.Equal(expected, result, ignoreLineEndingDifferences = true)
}

let t = new System.Threading.Tasks.ValueTask<_>(1)

[<Fact>]
let ``Test constructor call`` () = task {
    let fsharp = "let a () = new System.Collections.Generic.List<'a>()"
    let! result = TextCompiler.toLower fsharp

    let expected =
        "\
let a () =
    new System.Collections.Generic.List<'a>()
"

    do Assert.Equal(expected, result, ignoreLineEndingDifferences = true)
}

let a () = let x = 1 in x

[<Fact>]
let ``Test match call`` () = task {
    let fsharp = "let a () = match 1 with a -> a"
    let! result = TextCompiler.toLower fsharp

    let expected = $"let a () = let a = 1 in a{Environment.NewLine}"

    do Assert.Equal(expected, result, ignoreLineEndingDifferences = true)
}

[<Fact>]
let ``Test match call2`` () = task {
    let fsharp = "let a () = match None with | None -> 1 | Some (x: int) -> x"
    let! result = TextCompiler.toLower fsharp

    let expected =
        "\
let a () =
    let matchValue = Option.None in

    match matchValue with
    | Some(Value) ->
        let x = Value
        x
    | _ -> 1
"

    do Assert.Equal(expected, result, ignoreLineEndingDifferences = true)
}



[<Fact>]
let ``Test match call3`` () = task {
    let fsharp =
        "let a () = match Choice1Of3 () with Choice1Of3 _ -> 1 | Choice2Of3 _ -> 2 | Choice3Of3 _ -> 3"

    let! result = TextCompiler.toLower fsharp

    let expected =
        "\
let a () =
    let matchValue = Choice.Choice1Of3() in

    match matchValue with
    | Choice2Of3(Item) -> 2
    | Choice3Of3(Item) -> 3
    | _ -> 1
"

    do Assert.Equal(expected, result, ignoreLineEndingDifferences = true)
}

[<Fact>]
let ``Test match call3 when case`` () = task {
    let fsharp =
        "match None with | None -> 1 | Some (x: int) when x > 1 -> x | Some x -> let _ = ignore x in 2"

    let! result = TextCompiler.toLower fsharp

    let expected =
        "\
let matchValue = Option.None in

match matchValue with
| Some(Value) when
    let x = Value
    x > 1
    ->
    let x = Value in x
| Some(Value) ->
    let x = Value in
    ignore (x)
    2
| _ -> 1
"

    do Assert.Equal(expected, result, ignoreLineEndingDifferences = true)
}

[<Fact>]
let ``Test constructor call value type`` () = task {
    let fsharp = "let a () = new System.Threading.Tasks.ValueTask<float>()"
    let! result = TextCompiler.toLower fsharp

    let expected =
        "\
let a () =
    new System.Threading.Tasks.ValueTask<float>()
"

    do Assert.Equal(expected, result, ignoreLineEndingDifferences = true)
}

let e () =
    (fun (builder: TaskBuilder) ->
        builder.Run(builder.Delay(fun () -> builder.Bind(Task.Delay(1), (fun (_arg1: unit) -> builder.Zero())))))
        task

[<Fact>]
let ``Test task simple`` () = task {
    let fsharp =
        """
                task {
                    return 1
                }
            """

    let expr = <@@ task { return 1 } @@>

    let sanity = expr.Decompile()

    let expected =
        "\
    (fun (builder: TaskBuilder) -> builder.Run(builder.Delay(fun () -> builder.Return(1)))) task
"

    let! result = TextCompiler.toLower fsharp

    do Assert.Equal(expected, result)
}

[<Fact>]
let ``Test task do delay`` () = task {
    let fsharp = "let e () = task { do! System.Threading.Tasks.Task.Delay(1) }"
    let! result = TextCompiler.toLower fsharp

    let expected =
        "\
let e () =
    (fun (builder: TaskBuilder) ->
        builder.Run(builder.Delay(fun () -> builder.Bind(Task.Delay(1), fun () -> builder.Zero()))))
        task
"

    do Assert.Equal(expected, result, ignoreLineEndingDifferences = true)
}

[<Fact>]
let ``Test task use Dispose`` () = task {
    let fsharp =
        "\
let e (s: System.Threading.SemaphoreSlim) = task {
    use lock = s
    try
        do! lock.WaitAsync()
        return ()
    finally
        ignore (lock.Release(1))
}
"

    let! result = TextCompiler.toLower fsharp

    let expected =
        "\
let e (s: SemaphoreSlim) =
    (fun (builder: TaskBuilder) ->
        builder.Run(
            builder.Delay(fun () ->
                builder.Using(
                    s,
                    fun (_arg1: SemaphoreSlim) ->
                        let lock = _arg1

                        builder.TryFinally(
                            builder.Delay(fun () -> builder.Bind(lock.WaitAsync(), fun () -> builder.Return())),
                            fun () -> ignore (lock.Release(1))
                        )
                ))
        ))
        task
"

    do Assert.Equal(expected, result, ignoreLineEndingDifferences = true)
}

open System.Threading
open System.Threading.Tasks
open System.Collections.Generic

let ex (s: IAsyncEnumerable<'a>) (f) (builder: TaskBuilder) =
    (fun (builder: TaskBuilder) ->
        builder.Run(
            builder.Delay(fun () ->
                builder.Using(
                    s.GetAsyncEnumerator(CancellationToken.None),
                    fun (_arg1: IAsyncEnumerator<'a>) ->
                        let enumerator = _arg1
                        let mutable hasMore = true in

                        builder.Bind(
                            enumerator.MoveNextAsync(),
                            fun (_arg2: bool) ->
                                let more = _arg2
                                hasMore <- more

                                builder.While(
                                    (fun () -> hasMore),
                                    builder.Delay(fun () ->
                                        builder.Bind(
                                            (f) enumerator.Current :> Task<unit>,
                                            fun () ->
                                                builder.Bind(
                                                    enumerator.MoveNextAsync(),
                                                    fun (_arg4: bool) ->
                                                        let more = _arg4
                                                        hasMore <- more
                                                        builder.Zero()
                                                )
                                        ))
                                )
                        )
                ))
        ))
        task

[<Fact>]
let ``Test task use AsyncDispose`` () = task {
    let fsharp =
        "\
let e (s: System.Collections.Generic.IAsyncEnumerable<_>) f =
        task {
            use enumerator = s.GetAsyncEnumerator(System.Threading.CancellationToken.None)
            let mutable hasMore = true
            let! more = enumerator.MoveNextAsync()
            hasMore <- more
            while hasMore do
                do! f enumerator.Current

                let! more = enumerator.MoveNextAsync()
                hasMore <- more
    }
"

    let! result = TextCompiler.toLower fsharp

    let expected =
        "\
let e (s: IAsyncEnumerable<'a>) f =
    (fun (builder: TaskBuilder) ->
        builder.Run(
            builder.Delay(fun () ->
                builder.Using(
                    s.GetAsyncEnumerator(CancellationToken.None),
                    fun (_arg1: IAsyncEnumerator<'a>) ->
                        let enumerator = _arg1
                        let mutable hasMore = true in

                        builder.Bind(
                            enumerator.MoveNextAsync(),
                            fun (_arg2: bool) ->
                                let more = _arg2
                                hasMore <- more

                                builder.While(
                                    (fun () -> hasMore),
                                    builder.Delay(fun () ->
                                        builder.Bind(
                                            f enumerator.Current :> Task<unit>,
                                            fun () ->
                                                builder.Bind(
                                                    enumerator.MoveNextAsync(),
                                                    fun (_arg4: bool) ->
                                                        let more = _arg4
                                                        hasMore <- more
                                                        builder.Zero()
                                                )
                                        ))
                                )
                        )
                ))
        ))
        task
"

    do Assert.Equal(expected, result, ignoreLineEndingDifferences = true)
}

[<Fact>]
let ``Test scenario try with`` () = task {
    let fsharp =
        "\
module A
open CompExpr

let e() = 
    task {
        try
            do! System.Threading.Tasks.Task.Delay(1)
        with _e ->
            do ()
    }"

    let expected =
        "\
let e () =
    (fun (builder: TaskBuilder) ->
        builder.Run(
            builder.Delay(fun () ->
                builder.TryWith(
                    builder.Delay(fun () -> builder.Bind(Task.Delay(1), fun () -> builder.Zero())),
                    fun (_arg2: exn) ->
                        let _e = _arg2 in
                        ()
                        builder.Zero()
                ))
        ))
        task
"

    let! result = TextCompiler.toLower fsharp
    Assert.Equal(expected, result, ignoreLineEndingDifferences = true)
}

[<Fact>]
let ``Abcd`` () = task {
    let fsharp =
        "\
module A
open CompExpr

let rec fib x = tramp {
    let! a = fib (x - 1)
    let! b = fib (x - 2)
    return a + b
}"

    let expected =
        "\
let fib (x: int) =
    (fun (builder: TrampolineBuilder) ->
        builder.Delay(fun () ->
            builder.Bind(
                A.fib (x - 1),
                fun (_arg1: int) ->
                    let a = _arg1

                    builder.Bind(
                        A.fib (x - 2),
                        fun (_arg2: int) ->
                            let b = _arg2
                            builder.Return(a + b)
                    )
            )))
        tramp
"

    let! result = TextCompiler.toLower fsharp
    Assert.Equal(expected, result, ignoreLineEndingDifferences = true)
    return ()
}
