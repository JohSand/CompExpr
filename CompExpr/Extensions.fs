namespace FSharp.Control

open System.Threading.Tasks

[<AutoOpen>]
module Ext =
    type AsyncBuilder with

        member this.Bind(t: Task<_>, f) = this.Bind(Async.AwaitTask t, f)
