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
    static member toText([<ReflectedDefinition()>]a: Expr<_>) =
        a.Decompile()

[<Fact>]
let ``Test static creator method`` () =
    async {
        let! result = TextCompiler.toLower "System.Threading.Channels.Channel.CreateUnbounded<int>()"
        let expected = "let anon = Channel.CreateUnbounded<int>()\r\n"
        do Assert.Equal(expected, result)
    }


[<Fact>]
let ``Test csharp property accessor`` () =
    async {
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
            builder.Delay(
                fun () ->
                    let chan = Channel.CreateUnbounded<int>() in

                    builder.Bind(
                        chan.Reader.ReadAsync(new System.Threading.CancellationToken()),
                        fun (_arg1: int) ->
                            let reader = _arg1
                            chan.Writer.Complete(null)
                            builder.Zero()
                    )
            )
        ))
        task
"
        do Assert.Equal(expected, result)
    }


[<Fact>]
let ``Test variable let`` () =
    async {
        let expr =
            <@@
                let a = 1
                ()
            @@>

        let fsharp = expr.Decompile()
        let! result = TextCompiler.toLower fsharp
        let expected = "let anon = let a = 1 in ()\r\n"
        do Assert.Equal(expected, result)
    }

[<Fact>]
let ``Test plus operator`` () =
    async {
        let fsharp =
            Code.toText(
                1 + 2
            )
        let! result = TextCompiler.toLower fsharp
        let expected = "let anon = 1 + 2\r\n"
        do Assert.Equal(expected, result)
    }

[<Fact>]
let ``Test prefix operator`` () =
    async {
        let fsharp =
            Code.toText(~~~ 2)            

        let! result = TextCompiler.toLower fsharp
        let expected = "let anon = ~~~ 2\r\n"
        do Assert.Equal(expected, result)
    }

[<Fact>]
let ``Test simple lambda`` () =
    async {
        let fsharp =
            Code.toText(fun () -> ())            

        let! result = TextCompiler.toLower fsharp
        let expected = "let anon = fun () -> ()\r\n"
        do Assert.Equal(expected, result)
    }

[<Fact>]
let ``Test simple lambda with typed argument`` () =
    async {
        let fsharp =
            Code.toText(fun (a: int) -> a)            

        let! result = TextCompiler.toLower fsharp
        let expected = "let patternInput (a) = a\r\n"
        do Assert.Equal(expected, result)
    }

[<Fact>]
let ``Test simple lambda with two typed arguments`` () =
    async {
        let fsharp =
            Code.toText(fun (a: int) (b: int) -> a + b)

        let! result = TextCompiler.toLower fsharp
        let expected = "let anon = fun (x: int) -> fun (y: int) -> x + y\r\n"
        do Assert.Equal(expected, result)
    }

[<Fact>]
let ``Test single argument let`` () =
    async {
        let fsharp = "let f a = 1"
        let! result = TextCompiler.toLower fsharp
        let expected = "let f (a) = 1\r\n"
        do Assert.Equal(expected, result)
    }

[<Fact>]
let ``Test single argument unit let`` () =
    async {
        let fsharp = "let f () = 1"
        let! result = TextCompiler.toLower fsharp
        let expected = "let f () = 1\r\n"
        do Assert.Equal(expected, result)
    }

[<Fact>]
let ``Test single typed argument let`` () =
    async {
        let fsharp = "let f (a: int) = a"
        let! result = TextCompiler.toLower fsharp
        let expected = "let f (a: int) = a\r\n"
        do Assert.Equal(expected, result)
    }

[<Fact>]
let ``Test multi typed argument let`` () =
    async {
        let fsharp = "let f (a: int) (b: int) = a"
        let! result = TextCompiler.toLower fsharp
        let expected = "let f (a: int) (b: int) = a\r\n"
        do Assert.Equal(expected, result)
    }

[<Fact>]
let ``Test single typed generic argument let`` () =
    async {
        let fsharp = "let f (a: System.Collections.Generic.List<_>) (b: int) = a"
        let! result = TextCompiler.toLower fsharp
        let expected = "let f (a: List<'a>) (b: int) = a\r\n"
        do Assert.Equal(expected, result)
    }

[<Fact>]
let ``Test calling a curried function`` () =
    async {
        let fsharp =
            Code.toText(List.map id [])            

        let! result = TextCompiler.toLower fsharp
        let expected = "let anon = Microsoft.FSharp.Collections.List.map fun (x: obj) -> id (x) List.Empty\r\n"
        do Assert.Equal(expected, result)
    }

[<Fact>]
let ``Test lambda end parens`` () =
    async {
        let fsharp = "id (fun () -> ())"
        let! result = TextCompiler.toLower fsharp
        let expected = "let anon = id (fun () -> ())\r\n"
        do Assert.Equal(expected, result)
    }

let t = new System.Threading.Tasks.ValueTask<_>(1)

[<Fact>]
let ``Test constructor call`` () =
    async {
        let fsharp = "let a () = new System.Collections.Generic.List<'a>()"
        let! result = TextCompiler.toLower fsharp

        let expected =
            "\
let a () =
    new System.Collections.Generic.List<'a>()
"

        do Assert.Equal(expected, result)
    }

let a () = let x = 1 in x

[<Fact>]
let ``Test match call`` () =
    async {
        let fsharp = 
            "let a () = match 1 with a -> a"
        let! result = TextCompiler.toLower fsharp

        let expected =
            "let a () = let a = 1 in a\r\n"

        do Assert.Equal(expected, result)
    }

[<Fact>]
let ``Test match call2`` () =
    async {
        let fsharp = 
            "let a () = match None with | None -> 1 | Some (x: int) -> x"
        let! result = TextCompiler.toLower fsharp

        let expected =
            "\
let a () =
    let matchValue = Option.None  in

    match matchValue with
    | Some (Value) ->
        let x = Value
        x
    | _ -> 1
"

        do Assert.Equal(expected, result)
    }

[<Fact>]
let ``Test match call3`` () =
    async {
        let fsharp = 
            "let a () = match None with | None -> 1 | Some (x: int) when x > 1 -> x | Some x -> 2"
        let! result = TextCompiler.toLower fsharp

        let expected =
            "\
let a () =
    let matchValue = Option.None  in

    match matchValue with
    | Some (Value) when
        let x = Value
        x > 1
        ->
        let x = Value in x
    | Some (Value) ->
        let x = Value
        2
    | _ -> 1
"

        do Assert.Equal(expected, result)
    }

[<Fact>]
let ``Test constructor call value type`` () =
    async {
        let fsharp = "let a () = new System.Threading.Tasks.ValueTask<float>()"
        let! result = TextCompiler.toLower fsharp

        let expected =
            "\
let a () =
    new System.Threading.Tasks.ValueTask<float>()
"

        do Assert.Equal(expected, result)
    }

let e () =
    (fun (builder: TaskBuilder) ->
        builder.Run(builder.Delay(fun () -> builder.Bind(Task.Delay(1), fun (_arg1: unit) -> builder.Zero()))))
        task


[<Fact>]
let ``Test task do delay`` () =
    async {
        let fsharp = "let e () = task { do! System.Threading.Tasks.Task.Delay(1) }"
        let! result = TextCompiler.toLower fsharp

        let expected =
            "\
let e () =
    (fun (builder: TaskBuilder) ->
        builder.Run(builder.Delay(fun () -> builder.Bind(Task.Delay(1), (fun () -> builder.Zero())))))
        task
"

        do Assert.Equal(expected, result)
    }

[<Fact>]
let ``Test task use Dispose`` () =
    async {
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
            builder.Delay (fun () ->
                builder.Using(
                    s,
                    fun (_arg1: SemaphoreSlim) ->
                        let lock = _arg1

                        builder.TryFinally(
                            builder.Delay(fun () -> builder.Bind(lock.WaitAsync(), (fun () -> builder.Return()))),
                            fun () -> ignore (lock.Release(1))
                        )
                ))
        ))
        task
"

        do Assert.Equal(expected, result)
    }

open System.Threading
open System.Threading.Tasks
open System.Collections.Generic

let ex (s: IAsyncEnumerable<'a>) (f) (builder: TaskBuilder) =
    (fun (builder: TaskBuilder) ->
        builder.Run(
            builder.Delay (fun () ->
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
                                    builder.Delay (fun () ->
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
let ``Test task use AsyncDispose`` () =
    async {
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
let e (s: IAsyncEnumerable<'a>) (f) =
    (fun (builder: TaskBuilder) ->
        builder.Run(
            builder.Delay (fun () ->
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
                                    builder.Delay (fun () ->
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

        do Assert.Equal(expected, result)
    }

[<Fact>]
let ``Test scenario try with`` () =
    async {
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
            builder.Delay (fun () ->
                builder.TryWith(
                    builder.Delay(fun () -> builder.Bind(Task.Delay(1), (fun () -> builder.Zero()))),
                    fun (_arg2: exn) ->
                        let _e = _arg2 in
                        ()
                        builder.Zero()
                ))
        ))
        task
"

        let! result = TextCompiler.toLower fsharp
        Assert.Equal(expected, result)
    }

[<Fact>]
let ``Abcd``() = async {
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
        builder.Delay(
            fun () ->
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
                )
        ))
        tramp
"
    let! result = TextCompiler.toLower fsharp
    Assert.Equal(expected, result)
    return ()
}