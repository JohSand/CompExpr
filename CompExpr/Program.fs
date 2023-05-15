module CompExpr.Program

open FSharp.Control

open System.IO

async {
    let dir = Directory.GetCurrentDirectory() |> DirectoryInfo
    let scriptFile = Path.Combine(dir.FullName, "TestMe.fsx")
    let! content = File.ReadAllTextAsync(scriptFile)
    let! pp = TextCompiler.toLower content
    printfn "%s" pp
}
|> Async.RunSynchronously
