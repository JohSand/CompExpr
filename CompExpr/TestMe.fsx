#r "nuget: IcedTasks, 0.5.4"

open System.Threading.Tasks
open IcedTasks

let coldTask_dont_start_immediately () =
    task {
        do! Async.Sleep(100)
        match 1 with
        | 1 ->
            do! Async.Sleep(100)
            return 1
        | _ ->
            do! Async.Sleep(100)
            return 2
    }
