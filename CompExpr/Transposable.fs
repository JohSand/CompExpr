namespace CompExpr

//open System.Runtime.InteropServices
//open System.Threading.Tasks
//open System
//open System.Runtime.CompilerServices
//open System.Text

//type Default6 = class end
//type Default5 = class inherit Default6 end
//type Default4 = class inherit Default5 end
//type Default3 = class inherit Default4 end
//type Default2 = class inherit Default3 end
//type Default1 = class inherit Default2 end

//type Zero =
//    inherit Default1

//    static member        Zero (_: System.TimeSpan                , _: Zero    ) = System.TimeSpan ()
//    static member        Zero (_: list<'a>                       , _: Zero    ) = []   :   list<'a>
//    static member        Zero (_: option<'a>                     , _: Zero    ) = None : option<'a>
//    static member        Zero (_: array<'a>                      , _: Zero    ) = [||] :  array<'a>
//    static member        Zero (_: string                         , _: Zero    ) = ""
//    static member        Zero (_: StringBuilder                  , _: Zero    ) = new StringBuilder ()
//    static member        Zero (_: unit                           , _: Zero    ) = ()
//    static member        Zero (_: bool                           , _: Zero    ) = false
//    static member        Zero (_: Set<'a>                        , _: Zero    ) = Set.empty : Set<'a>
//    static member        Zero (_: Map<'a,'b>                     , _: Zero    ) = Map.empty : Map<'a,'b>

//    static member inline Invoke () =
//        let inline call_2 (a: ^a, b: ^b) = ((^a or ^b) : (static member Zero : _*_ -> _) b, a)
//        let inline call (a: 'a) = call_2 (a, Unchecked.defaultof<'r>) : 'r
//        call Unchecked.defaultof<Zero>

//[<Extension; Sealed>]
//type Plus =
//    inherit Default1
//    static member inline ``+`` (x: 'Plus             , y: 'Plus             ,             _mthd: Default2) = (^Plus :  (static member (<|>) : _*_ -> _) x, y) : ^Plus

//    static member inline ``+`` (x: 'Plus             , y: 'Plus             , [<Optional>]_mthd: Default1) = x + y : ^Plus
//    static member inline ``+`` (_: ^t when ^t: null and ^t: struct, _: ^t   , [<Optional>]_mthd: Default1) = id

//    static member        ``+`` (x: list<_>           , y                    , [<Optional>]_mthd: Plus    ) = x @ y
//    static member        ``+`` (x: array<_>          , y                    , [<Optional>]_mthd: Plus    ) = Array.append x y
//    static member        ``+`` (x: _ seq             , y                    , [<Optional>]_mthd: Default3) = Seq.append x y

//    static member inline Invoke (x: 'Plus) (y: 'Plus) : 'Plus =
//        let inline call (mthd : ^M, input1 : ^I, input2 : ^I) = ((^M or ^I) : (static member ``+`` : _*_*_ -> _) input1, input2, mthd)
//        call (Unchecked.defaultof<Plus>, x, y)

//type Plus with
//    static member inline ``+`` (x: option<_>         , y                    , [<Optional>]_mthd: Plus) =
//                    match x, y with
//                    | (Some a , Some b) -> Some (Plus.Invoke a b)
//                    | (Some a , None  ) -> Some a
//                    | (None   , Some b) -> Some b
//                    | _                 -> None

//type Fold =
//    inherit Default1


//    //static member inline Fold (x: 'F       , f: 'b->'a->'b, z: 'b, [<Optional>]_impl: Default1) = (^F : (static member Fold : ^F -> _ -> _-> ^b) x, f, z)
//    static member        Fold (x: option<_>, f,             z    , [<Optional>]_impl: Fold    ) = match x with Some x -> f z x | _ -> z
//    //static member        Fold (x: seq<_>   , f,             z    , [<Optional>]_impl: Fold    ) = Seq.fold               f z x
//    static member        Fold (x: list<_>  , f,             z    , [<Optional>]_impl: Fold    ) = List.fold              f z x
//    static member        Fold (x: Set<_>   , f,             z    , [<Optional>]_impl: Fold    ) = Set.fold               f z x
//    static member        Fold (x:  _ []    , f,             z    , [<Optional>]_impl: Fold    ) = Array.fold             f z x

//    static member inline Invoke (folder: 'State->'T->'State) (state: 'State) (foldable: '``Foldable'<T>``) : 'State =
//        let inline call_2 (a: ^a, b: ^b, f, z) = ((^a or ^b) : (static member Fold : _*_*_*_ -> _) b, f, z, a)
//        let inline call (a: 'a, b: 'b, f, z) = call_2 (a, b, f, z)
//        call (Unchecked.defaultof<Fold>, foldable, folder, state)

//type Bind =
//    static member        (>>=) (source: Lazy<'T>   , f: 'T -> Lazy<'U>    ) = lazy (f source.Value).Value             : Lazy<'U>
//    static member        (>>=) (source: seq<'T>    , f: 'T -> seq<'U>     ) = Seq.collect f source                    : seq<'U>
//    static member        (>>=) (source: Task<'T>   , f: 'T -> Task<'U>    ) = 
//        let fInner = Func<Task<'T>,Task<'U>>(fun (t: Task<'T>) -> (f t.Result))
//        source.ContinueWith(fInner).Unwrap()                  : Task<'U>

//    static member        (>>=) (source: Nullable<_>, f: 'T -> _           ) = 
//        if source.HasValue then f source.Value else Nullable()                : Nullable<'U>
//    static member        (>>=) (source             , f: 'T -> _           ) = Option.bind   f source                  : option<'U>
//    static member        (>>=) (source             , f: 'T -> _           ) = List.collect  f source                  : list<'U>
//    static member        (>>=) (source             , f: 'T -> _           ) = Array.collect f source                  : 'U []
//    static member        (>>=) (source             , k: 'T -> _           ) = (fun r -> k (source r) r)               : 'R->'U
//    static member        (>>=) (source             , f: 'T -> _           ) = async.Bind (source, f)                 : Async<'U>
//    static member        (>>=) (source             , k: 'T -> _           ) = Result.bind k source                   : Result<'U,'E>

//    static member inline Invoke (source: '``Monad<'T>``) (binder: 'T -> '``Monad<'U>``) : '``Monad<'U>`` =
//        let inline call (_mthd: 'M, input: 'I, _output: 'R, f) = ((^M or ^I or ^R) : (static member (>>=) : _*_ -> _) input, f)
//        call (Unchecked.defaultof<Bind>, source, Unchecked.defaultof<'``Monad<'U>``>, binder)

//type Return =
//    inherit Default1
//    static member inline InvokeOnInstance (x: 'T) = (^``Applicative<'T>`` : (static member Return : ^T -> ^``Applicative<'T>``) x)
               
//    static member inline Invoke (x: 'T) : '``Applicative<'T>`` =
//        let inline call (mthd: ^M, output: ^R) = ((^M or ^R) : (static member Return : _*_ -> _) output, mthd)
//        call (Unchecked.defaultof<Return>, Unchecked.defaultof<'``Applicative<'T>``>) x
                               
//    static member        Return (_: seq<'a>        , _: Default2) = fun  x      -> Seq.singleton x : seq<'a>
//    static member inline Return (_: 'R             , _: Default1) = fun (x: 'T) -> Return.InvokeOnInstance x         : 'R
//    static member        Return (_: Lazy<'a>       , _: Return  ) = fun x -> Lazy<_>.CreateFromValue x : Lazy<'a>
//    static member        Return (_: 'T Task        , _: Return  ) = fun x -> Task.FromResult x                    : 'T Task
//    static member        Return (_: option<'a>     , _: Return  ) = fun x -> Some x                               : option<'a>
//    static member        Return (_: list<'a>       , _: Return  ) = fun x -> [ x ]                                : list<'a>
//    static member        Return (_: 'a []          , _: Return  ) = fun x -> [|x|]                                : 'a []
//    static member        Return (_: 'a Async       , _: Return  ) = fun (x: 'a) -> async.Return x
//    static member        Return (_: Result<'a,'e>  , _: Return  ) = fun x -> Ok x                                 : Result<'a,'e>
//    static member        Return (_: Choice<'a,'e>  , _: Return  ) = fun x -> Choice1Of2 x                         : Choice<'a,'e>
//    static member        Return (_: ResizeArray<'a>, _: Return  ) = fun x -> ResizeArray<'a> (Seq.singleton x)

//type Map =
//    inherit Default1


//    static member Map ((x: option<_>           , f: 'T->'U), _mthd: Map) = Option.map  f x
//    static member Map ((x: list<_>             , f: 'T->'U), _mthd: Map) = List.map    f x : list<'U>
//    static member Map ((g: 'R->'T              , f: 'T->'U), _mthd: Map) = (>>) g f
//    static member Map ((g: Func<'R, 'T>        , f: 'T->'U), _mthd: Map) = Func<'R, 'U> (g.Invoke >> f)
//    static member Map (((m: 'Monoid, a)        , f: 'T->'U), _mthd: Map) = (m, f a)
//    static member Map ((x: _ []                , f: 'T->'U), _mthd: Map) = Array.map   f x
//    static member Map ((x: _ [,]               , f: 'T->'U), _mthd: Map) = Array2D.map f x
//    static member Map ((x: _ [,,]              , f: 'T->'U), _mthd: Map) = Array3D.map f x
//    static member Map ((x: _ [,,,]             , f: 'T->'U), _mthd: Map) = Array4D.init (x.GetLength 0) (x.GetLength 1) (x.GetLength 2) (x.GetLength 3) (fun a b c d -> f x.[a,b,c,d])
//    static member Map ((x: Result<_,'E>        , f: 'T->'U), _mthd: Map) = Result.map f x
//    // Restricted
//    //static member Map ((x: string              , f        ), _mthd: Map) = String.map f x
//    //static member Map ((x: Set<_>              , f        ), _mthd: Map) = Set.map f x


//    static member inline Invoke (mapping: 'T->'U) (source: '``Functor<'T>``) : '``Functor<'U>`` = 
//        let inline call (mthd: ^M, source: ^I, _output: ^R) = ((^M or ^I or ^R) : (static member Map : (_*_)*_ -> _) (source, mapping), mthd)
//        call (Unchecked.defaultof<Map>, source, Unchecked.defaultof<'``Functor<'U>``>)

//[<AutoOpen>]
//module Operators =
//    /// <summary>
//    /// Lifts a value into a Functor. Same as return in Computation Expressions.
//    /// </summary>
//    /// <category index="2">Applicative</category>
//    let inline result (x: 'T) : '``Functor<'T>`` = Return.Invoke x
    
//    /// <summary>
//    /// Lifts a function into a Functor. Same as map but with flipped arguments.
//    /// To be used in pipe-forward style expressions
//    /// </summary>
//    /// <category index="1">Functor</category>
//    let inline (|>>) (x: '``Functor<'T>``) (f: 'T->'U) : '``Functor<'U>`` = Map.Invoke f x

//    /// <summary>
//    /// Takes a monadic value and a function from a plain type to a monadic value, and returns a new monadic value.
//    /// </summary>
//    /// <category index="3">Monad</category>
//    let inline (>>=) (x: '``Monad<'T>``) (f: 'T->'``Monad<'U>``) : '``Monad<'U>`` = Bind.Invoke x f

//    /// <summary>Applies a function to each element of the foldable, threading an accumulator argument
//    /// through the computation. Take the second argument, and apply the function to it
//    /// and the first element of the foldable. Then feed this result into the function along
//    /// with the second element and so on. Return the final result.
//    /// If the input function is <c>f</c> and the elements are <c>i0...iN</c> then 
//    /// computes <c>f (... (f s i0) i1 ...) iN</c>.</summary>
//    /// <category index="11">Foldable</category>
//    /// <param name="folder">The function to update the state given the input elements.</param>
//    /// <param name="state">The initial state.</param>
//    /// <param name="foldable">The input foldable.</param>
//    /// <returns>The final state value.</returns>
//    let inline fold (folder: 'State->'T->'State) (state: 'State) (foldable: '``Foldable<'T>``) : 'State = 
//        Fold.Invoke folder state foldable

//    /// <summary>
//    /// Gets a value that represents the 0 element of a Monoid.
//    /// </summary>
//    /// <category index="4">Monoid</category>
//    let inline getZero () : 'Monoid = Zero.Invoke ()

//    /// <summary>
//    /// A value that represents the 0 element of a Monoid.
//    /// </summary>
//    /// <category index="4">Monoid</category>
//    let inline zero< ^Monoid when (Zero or ^Monoid) : (static member Zero : ^Monoid * Zero -> ^Monoid) > : ^Monoid = Zero.Invoke ()

//type Transposable =
//    //static member inline In(instance: '``M<'a>`` option) : '``M<'a option>`` = 
//    //    match instance with
//    //    | Some a -> a |>> Some
//    //    | None -> result (None)        

//    static member inline In(instance: Result<'``M<'a>``, 'err>) : '``M<Result<'a, 'err>>`` = 
//        match instance with
//        | Ok a -> a |>> Ok
//        | Error e-> result (Error e)

//    //F kräver Fold och Zero
//    //M -> Monad
//    static member inline In(instance: '``F<M<'a>>``) : '``M<F<'a>>`` =
//        let (seed: 'a option) = getZero()
//        instance
//        |> fold 
//           (fun (agg: '``M<F<'a>>``) (curr: '``M<'a>``) -> 
//                    agg >>= 
//                    (fun (l: '``F<'a>``) -> 
//                        curr 
//                        |>> (fun (a: 'a) -> 
//                                let (otherList) = result (a)
//                                Plus.Invoke l otherList 
//                            )
//                    )
//            ) 
//           (result (seed))
//    //static member inline In(instance: '``M<'a>`` list) : '``M<list<'a>>`` = 
//    //    instance
//    //    |> fold 
//    //        (fun agg curr -> agg >>= (fun l -> curr |>> (fun a -> a :: l))) 
//    //        (result [])


//module Sanity =
//    let s1 =
//        let test = Some ([1])
//        let test2 = Transposable.In(test)
//        ()


//type MyGenericType<'a> =
//    static member Test() = ()


//type MyGenericType<int> with
//    static member Abc() = ()
