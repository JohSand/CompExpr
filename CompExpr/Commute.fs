module Commute

type Pair<'a> = P of ('a * 'a)

type Pair<'A> with
    static member Return(a: 'a) = P (a, a)
    static member Map((P (a, b)): Pair<'a>, f: 'a -> 'b) : Pair<'b> = P (f a, f b)

    static member Bind((P (a, b)): Pair<'a>, f: 'a -> Pair<'b>) : Pair<'b> =
        let (P (x, _)) = f a
        let (P (_, y)) = f b
        P(x, y)


type Bad<'a> = B of (Option<Pair<'a>>)

type Bad<'A> with
    static member Return(a: 'a) : Bad<'a> = B(Some(P(a, a)))

    static member Map((P (a, b)): Pair<'a>, f: 'a -> 'b) : Pair<'b> = P(f a, f b)

    static member Bind((P (a, b)): Pair<'a>, f: 'a -> Pair<'b>) : Pair<'b> =
        let (P (x, _)) = f a
        let (P (_, y)) = f b
        P(x, y)