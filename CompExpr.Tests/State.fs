namespace CompExpr

open System.Threading.Tasks
open FSharp.Control


[<NoComparison; NoEquality>]
type State<'T> = State of (unit -> System.Threading.Tasks.Task<unit * 'T>)

module State =
    let read = State <| fun state -> Task.FromResult(state, state)

    let set state =
        State <| fun _ -> Task.FromResult(state, ())

    let ret v = State(fun s -> Task.FromResult(s, v))

    let bind (f: 'a -> State<'b>) ((State s): State<'a>) : State<'b> =
        State(fun ss ->
            task {
                let! (next, a) = s ss
                return! let (State b) = f a in b next
            })

    let map (f: 'a -> 'b) (s: State<'a>) : State<'b> = bind (f >> ret) s

    let map2 f s1 s2 = bind (fun a -> map (f a) s2) s1

    let combine s1 s2 = map2 (fun _ -> id) s1 s2

type ScenarioStateBuilder() =
    member _.Return(v) : State<'T> = State.ret v
    member _.ReturnFrom a = a
    member _.Bind(s, f) = State.bind f s

    member _.For(s: seq<'a>, f: 'a -> State<unit>) : State<unit> =
        s |> Seq.map f |> Seq.reduce State.combine

    member this.While(guard, f) : State<unit> =
        if not (guard ()) then
            this.Zero()
        else
            this.Bind(f (), (fun () -> this.While(guard, f)))

    member _.Zero() = State.ret ()
    member _.Combine(s1: State<unit>, s2) = State.bind s2 s1
    member _.Delay(f: unit -> State<'a>) = f
    member _.Run(f: unit -> State<'a>) = f ()

    member _.TryWith(body: _ -> State<'a>, handler) : State<'a> =
        let ((State s): State<'a>) = body ()

        State(fun ss ->
            task {
                try
                    return! s ss
                with e ->
                    //let! cont = handler e
                    return! let (State cont) = (handler e) in cont ss
            })


module StateExtensions =
    let scenario = ScenarioStateBuilder()
    let generateScenario () : State<unit> = failwith ""

[<AbstractClass>]
type Trampoline<'a> internal () =
    abstract member Eval: unit -> 'a

type private Return<'a>(a: 'a) =
    inherit Trampoline<'a>()
    override this.Eval() = a

type private Suspend<'a>(cont: unit -> Trampoline<'a>) =
    inherit Trampoline<'a>()
    override this.Eval() = cont().Eval()

type private Bind<'a, 'b>(current: Trampoline<'a>, cont: 'a -> Trampoline<'b>) =
    inherit Trampoline<'b>()

    override this.Eval() =
        let a = current.Eval()
        (cont a).Eval()

module Trampoline =

    let ret (a: 'a) : Trampoline<'a> = Return a

    let bind (f: 'a -> Trampoline<'b>) (t: Trampoline<'a>) : Trampoline<'b> = Bind<_, _>(t, f)

type TrampolineBuilder() =
    member this.Bind(a: Trampoline<'a>, f) = a |> Trampoline.bind f

    member this.Return(a: 'a) = a |> Trampoline.ret

    member this.Delay(f) = Suspend(f) :> Trampoline<'a>


[<AutoOpen>]
module TrampolineBuilder =
    let tramp = TrampolineBuilder()
