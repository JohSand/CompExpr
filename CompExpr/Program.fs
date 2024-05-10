module CompExpr.Program

open FSharp.Control

open System.IO
open System.Threading
open System
open System.Threading.Tasks
open System.Runtime.CompilerServices
open System.Security.Cryptography

//async {
//    let dir = Directory.GetCurrentDirectory() |> DirectoryInfo
//    let scriptFile = Path.Combine(dir.FullName, "TestMe.fsx")
//    let! content = File.ReadAllTextAsync(scriptFile)
//    let! pp = TextCompiler.toLower content
//    File.WriteAllText(Path.Combine(dir.FullName, "Desugared.fsx"), pp)
//}
//|> Async.RunSynchronously

let mutable lastFileSize = 0L

let waitOnChanged (watcher: FileSystemWatcher) (t: CancellationToken) =
    let tcs = TaskCompletionSource<FileSystemEventArgs>()
    
    let reg = 
        t.Register(fun () -> 
            tcs.SetCanceled(t)
        )
    let mutable forwardDeclare = Unchecked.defaultof<FileSystemEventHandler>
    forwardDeclare <- 
        FileSystemEventHandler(fun _o e ->
            watcher.Changed.RemoveHandler(forwardDeclare)
            tcs.SetResult(e)
            reg.Dispose()
        )

    watcher.Changed.AddHandler(forwardDeclare)
    tcs.Task

let tryFindOpenStatements (loweredFile: string) (basefile: string) =
    //let s = loweredFile.IndexOf(Environment.NewLine)
    let openingLetStmt = loweredFile[0..(loweredFile.IndexOf(Environment.NewLine) - 1)]
    basefile[0..(basefile.IndexOf(openingLetStmt) - 1)]
    

let writeDesugared (path) = task {
    try
        let file = FileInfo(path)
        //naïve
        if file.Length <> Interlocked.Exchange(&lastFileSize, file.Length) then      
            use fs = file.Open(FileMode.Open, FileAccess.Read, FileShare.ReadWrite)
            use reader = new StreamReader(fs)
            let! content = reader.ReadToEndAsync()
            Console.WriteLine("creating lowered output...")
            let! pp = TextCompiler.toLower content
            let opn = tryFindOpenStatements pp content
            File.WriteAllText(Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments), "Desugared.fsx"), opn + pp)
            Console.WriteLine("wrote lowered output...")
    with
    | exn ->
        Console.WriteLine(exn)
        Console.WriteLine("file buzy, retrying...")
        do! Task.Delay(100)
        //meh, but only limited depth
        return ()

}




let asyncMain (dir: string) = backgroundTask {
    use cts = new CancellationTokenSource()
    Console.CancelKeyPress.Add(fun (a: ConsoleCancelEventArgs) ->
        a.Cancel <- true
        cts.Cancel()
    )
    Console.WriteLine($"Watching folder {dir}")
    let files = DirectoryInfo(dir).GetFiles("*.fsx")
    Console.WriteLine($"Files in scope:")
    for file in files do
        Console.WriteLine(file.FullName)
    
    use watcher = new FileSystemWatcher(dir)
    watcher.EnableRaisingEvents <- true
    watcher.Filter <- "*.fsx"
    try
        while (not cts.Token.IsCancellationRequested) do
            let! args = waitOnChanged watcher cts.Token
            Console.WriteLine("Got change.")
            do! writeDesugared (args.FullPath)
            
    with
    | :? OperationCanceledException ->
        ()

    Console.WriteLine("Exiting...")

    return ()
}

[<EntryPoint>]
let main args =
    let dir =
        if args.Length > 0 then
            args[0]
        else
            //Directory.GetCurrentDirectory()
            "C:\Projects\JohSand.TechTop\Cexpr"

    asyncMain(dir).GetAwaiter().GetResult()
    0
