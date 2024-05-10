#r "nuget: IcedTasks, 0.5.4"

open System.Threading.Tasks
open IcedTasks

let coldTask_dont_start_immediately () =
    task {
        let mutable someValue = 0
        let fooColdTask = coldTask { someValue <- 42 }
        // ColdTasks will not execute until they are called, similar to how Async works
        // Calling fooColdTask will start to execute it
        do! fooColdTask ()
    }
