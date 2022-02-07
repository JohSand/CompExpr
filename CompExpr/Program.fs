module CompExpr.Program

open System.Threading.Tasks
open FSharp.Control
open System.Collections.Generic

let fsharp =
    "\
module A
open CompExpr

let e() = 
    env {
        let! a = ConsoleService.readLine()
        and! b = ConsoleService.readLine()

        return (a, b)
    }
    "
let (a: seq<int>) = [ 1]

let ttest =
    task {
        do! Async.Sleep(1)
        return ()
    }

let e() =
    (fun (builder: EnvBuilder) ->
        builder.Run(
            builder.Delay (fun () ->
                builder.Bind2Return(
                    CompExpr.ConsoleService.readLine (),
                    CompExpr.ConsoleService.readLine (),
                    fun (_arg1: _ * _) ->
                        let b = _arg1.Item2
                        let a = _arg1.Item1
                        a, b
                ))
        ))
        env

async {
    match! Helpers.getTypedParseTree fsharp with
    | Ok ([ decls ]) ->    
        let! pp = 
            decls 
            |> Mapper.getUntypedParseTree 
            |> Helpers.writeFormated
        printfn "%s" pp    
    | Error s -> failwithf "%s" s
    | _ -> failwith ""
} |> Async.RunSynchronously
