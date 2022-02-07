namespace CompExpr

open FSharp.Control
open System.Threading.Tasks
open System.Runtime.InteropServices
open System.Runtime.CompilerServices
open Microsoft.FSharp.Core.CompilerServices


type AsyncResult<'a, 'e> = ValueTask<Result<'a, 'e>>

type EffectFunc<'r, 'a, 'e> = delegate of 'r -> AsyncResult<'a, 'e>

[<NoEquality; NoComparison; Struct>]
type Effect<'r, 'a, 'e> = Eff of EffectFunc<'r, 'a, 'e>

[<RequireQualifiedAccess>]
module Effect =
    let run env (Eff e) = e.Invoke env

    let inline eff f = Eff(EffectFunc(f))

    let inline effT (f: 'r -> Task<Result<'a, 'e>>) =
        Eff(EffectFunc(f >> ValueTask<Result<_, _>>))

    let ret<'r, 'a, 'e> (a: 'a) : Effect<'r, 'a, 'e> =
        Eff(EffectFunc(fun (_env: 'r) -> ValueTask.FromResult(Ok a)))

    let bind (f: 'a -> Effect<'r, 'b, 'e>) (env: Effect<'r, 'a, 'e>) =
        effT
            (fun s ->
                task {
                    match! run s env with
                    | Ok a -> return! run s (f a)
                    | Error e -> return Error e
                })

    let bind2 (f: 'a -> Effect<'r, 'b, 'e2>) (e: Effect<'r, 'a, 'e1>) =
        effT
            (fun env ->
                task {
                    match! run env e with
                    | Ok a ->
                        let! b2 = run env (f a)
                        return b2 |> Result.mapError (Choice2Of2)
                    | Error e -> return Error(Choice1Of2 e)
                })

    let inline (>>=) m f = bind f m

    let map (f: 'a -> 'b) (env: Effect<'r, 'a, 'e>) =
        effT
            (fun s ->
                task {
                    match! run s env with
                    | Ok a -> return Ok(f a)
                    | Error e -> return Error e
                })

    let map2 f s1 s2 = bind (fun a -> map (f a) s2) s1

    let plyPair (a: 'a ValueTask) (b: 'b ValueTask) =
        task {
            let! a' = a
            let! b' = b
            return (a', b')
        }

    let inline zip (m1: Effect<'r, 'a, 'e>) (m2: Effect<'r, 'b, 'e>) (f: ('a * 'b) -> 'g) =
        effT
            (fun s ->
                task {
                    let a' = run s m1
                    let b' = run s m2

                    match! plyPair a' b' with
                    | Ok a, Ok b -> return Ok(f (a, b))
                    | Error e, _ -> return Error e
                    | _, Error e -> return Error e
                })

    let inline zipBind (m1: Effect<'r, 'a, 'e>) (m2: Effect<'r, 'b, 'e>) (f: ('a * 'b) -> Effect<'r, 'g, 'e>) =
        effT
            (fun s ->
                task {
                    let a' = run s m1
                    let b' = run s m2

                    match! plyPair a' b' with
                    | Ok a, Ok b -> return! run s (f (a, b))
                    | Error e, _ -> return Error e
                    | _, Error e -> return Error e
                })

    let joinResult (e: Effect<'r, Result<'a, 'e>, 'e>) =
        effT
            (fun s ->
                task {
                    match! run s e with
                    | Ok a -> return a
                    | Error e -> return Error e
                })

    let joinTask (e: Effect<'r, Task<'a>, 'e>) : Effect<'r, 'a, 'e>=
        effT
            (fun s ->
                task {
                    match! run s e with
                    | Ok a -> let! aTask = a in return Ok aTask
                    | Error e -> return Error e
                })

type Effect =
    static member Create(x: ('a -> 'b), [<System.ParamArray>] _lowest: int array) : Effect<'a, 'b, _> =
        Eff(x >> Ok >> ValueTask.FromResult)

    static member Create(full: ('a -> AsyncResult<'b, 'e>)) = Eff(full)

    static member Create(x: ('a -> Async<'b>), [<Optional>] _medium: byte) : Effect<'a, 'b, _> =
        Effect.effT
            (fun a ->
                task {
                    let! b = x a
                    return Ok b
                })


    static member Create(x: ('a -> Task<'b>), [<Optional>] _medium: byte) : Effect<'a, 'b, _> =
        Effect.effT
            (fun a ->
                task {
                    let! b = x a
                    return Ok b
                })

    static member Create(x: ('a -> ValueTask<'b>)) : Effect<'a, 'b, _> =
        Effect.eff
            (fun a ->
                let vTask = x a

                if vTask.IsCompletedSuccessfully then
                    ValueTask<Result<_, _>>(Ok vTask.Result)
                else
                    ValueTask<Result<_, _>>(
                        task {
                            let! result = vTask
                            return Ok result
                        }
                    )

                )



    static member Create(x: ('a -> Result<'b, 'e>)) = Eff(x >> ValueTask.FromResult)
type EnvBuilder() =
    member _.Bind(eff, f) = Effect.bind f eff
    member _.Return(a) = Effect.ret a
    member _.ReturnFrom(a: Effect<_,_,_>) = a
    member _.ReturnFrom(a: Result<_,_>) = Effect.Create(fun _a -> a)

    //member _.Bind2(a, b, f) = Effect.zipBind a b f

    //member __.BindReturn(a, f) = Effect.map f a
    member _.Zero() = Effect.ret ()
    member _.Yield(value) = Effect.ret value
    member __.Bind2Return(a, b, f) = Effect.zip a b f

    member _.MergeSources(a, b) = Effect.zip a b id

    member _.Delay (f: unit -> Effect<'s, 'a, 'e>) = f

    member _.Run f = f()

    //dunno why
    member _.Combine(s1: Effect<'s, 'b, 'e>, f: 'b -> Effect<'s, 'a, 'e>) = 
        Effect.bind f s1
        //Effect.map2 (fun a b -> (a, b)) s1 s2

    member _.While(guard, f) : Effect<'s, unit, 'e> = 
        Effect.effT
            (fun c ->
                task {
                    let mutable faulted = false
                    let mutable err = Unchecked.defaultof<_>
                    while guard() && (not faulted) do
                        match! Effect.run c (f ()) with
                        | Ok () -> ()
                        | Error e -> 
                            faulted <- true
                            err <- e
                            
                    if faulted then
                        return Error err
                    else
                        return Ok()
                })

    member _.For(xs: System.Collections.Generic.IEnumerable<'a>, f: ('a -> Effect<_, unit, 'e>)) =
        Effect.effT
            (fun c ->
                task {
                    use enumerator = xs.GetEnumerator()
                    let mutable faulted = false
                    let mutable err = Unchecked.defaultof<_>

                    while enumerator.MoveNext() && (not faulted) do
                        match! Effect.run c (f enumerator.Current) with
                        | Ok () -> ()
                        | Error e -> 
                            faulted <- true
                            err <- e
    

                    if faulted then
                        return Error err
                    else
                        return Ok()
                })

    member __.For(xs: System.Collections.Generic.IAsyncEnumerable<'a>, f: ('a -> Effect<_, unit, 'e>)) =
        Effect.effT
            (fun c ->
                task {
                    use enumerator = xs.GetAsyncEnumerator()
                    let! next = enumerator.MoveNextAsync()
                    let mutable moveNext = next
                    let mutable faulted = false
                    let mutable err = Unchecked.defaultof<_>

                    while moveNext && not faulted do
                        match! Effect.run c (f enumerator.Current) with
                         | Ok () -> ()
                         | Error e -> 
                             faulted <- true
                             err <- e
                        let! next = enumerator.MoveNextAsync()
                        moveNext <- next

                    if faulted then
                        return Error err
                    else
                        return Ok()
                })

    member this.TryWith(body: _ -> Effect<_,_,_>, handler) =
        try this.ReturnFrom(body())
        with e -> handler e

    member this.TryFinally(body: _ -> Effect<_,_,_>, compensation) =
        try this.ReturnFrom(body())
        finally compensation()


    //todo tie-breaker, if both disp and iasyncdicp
    member this.Using(disposable:#System.IAsyncDisposable, body) =
        Effect.effT
            (fun c ->
                task {
                    use a = disposable
                    return! Effect.run c (body a)
                })

[<AutoOpen>]
module EffectBuilder =
    let env = EnvBuilder()

open System
open System.IO

type ILoggingService =
    abstract Log: string -> unit

type IConsoleService =
    abstract WriteLine: string -> Result<unit, exn>
    abstract ReadLine: unit -> Result<string, exn>
    
type IPersistenceService =
    abstract Persist: 'a -> ValueTask<int>

type ILoggingServiceProvider =
    abstract LoggingService : ILoggingService

module LoggingService =
    let log a =
        Effect.Create(fun (env: #ILoggingServiceProvider) -> env.LoggingService.Log(a))

type IConsoleServiceProvider =
    abstract ConsoleService : IConsoleService

module ConsoleService =
    let writeLine a =
        Effect.Create(fun (env: #IConsoleServiceProvider) -> env.ConsoleService.WriteLine(a))

    let readLine () =
        Effect.Create(fun (env: #IConsoleServiceProvider) -> env.ConsoleService.ReadLine())


type IPersistenceServiceProvider =
    abstract PersistenceService : IPersistenceService

module PersistenceService =
    let persist a =
        Effect.Create(fun (env: #IPersistenceServiceProvider) -> env.PersistenceService.Persist(a))

type TestEnv() =

    interface IConsoleServiceProvider with
        member val ConsoleService =
            { new IConsoleService with
                member __.WriteLine s = Console.WriteLine(s) |> Ok
                member __.ReadLine() = Ok(Console.ReadLine()) }

    interface ILoggingServiceProvider with
        member val LoggingService =
            { new ILoggingService with
                member __.Log(s) = Console.WriteLine(s) }

    interface IPersistenceServiceProvider with
        member this.PersistenceService =
            { new IPersistenceService with
                member __.Persist s =
                    failwith ""
                    }

[<AutoOpen>]
module TestEnv =
    let testEnv = TestEnv()

[<AutoOpen>]
module LowPriority =
    type EnvBuilder with
        member inline this.Using(disposable:#System.IDisposable, body) =
            let body' = fun () -> body disposable
            this.TryFinally(body', fun () ->
                match disposable with
                    | null -> ()
                    | disp -> disp.Dispose())

        //Join Extensions
        //member _.Bind(task1: Task<'a>, f: 'a -> Effect<'r, 'b, 'e>) : Effect<'r, 'b, 'e> =
        //    failwith ""
        //special case for unit =(        
        member inline this.Bind(t, f: 'a -> Effect<'r, 'b, 'e>) : Effect<'r, 'b, 'e> =
            Effect.ret t
            |> Effect.joinResult 
            |> Effect.bind f
        member inline this.Bind(t, f: 'a -> Effect<'r, 'b, 'e>) : Effect<'r, 'b, 'e> =
            Effect.ret t
            |> Effect.joinTask 
            |> Effect.bind f
            //failwith ""
        
        //member this.Bind(task1: Task, f: unit -> Effect<'r, 'b, 'e>) : Effect<'r, 'b, 'e> =
        //    let eff = Effect.ret<'r, Task<unit>,'e> (task { return! task1 }) |> Effect.joinTask
        //    this.Bind(eff , f)