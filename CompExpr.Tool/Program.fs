module CompExpr.Program

open System
open System.IO
open System.Threading
open System.Threading.Tasks

let mutable lastFileSize = 0L

let waitOnChanged (watcher: FileSystemWatcher) (t: CancellationToken) =
    let tcs = TaskCompletionSource<FileSystemEventArgs>()

    let reg = t.Register(fun () -> tcs.SetCanceled(t))
    let mutable forwardDeclare = Unchecked.defaultof<FileSystemEventHandler>

    forwardDeclare <-
        FileSystemEventHandler(fun _o e ->
            watcher.Changed.RemoveHandler(forwardDeclare)
            tcs.SetResult(e)
            reg.Dispose())

    watcher.Changed.AddHandler(forwardDeclare)
    tcs.Task

let tryFindOpenStatements (loweredFile: string) (basefile: string) =
    //let s = loweredFile.IndexOf(Environment.NewLine)
    let openingLetStmt = loweredFile[0 .. (loweredFile.IndexOf("=") - 1)]

    basefile[0 .. (basefile.IndexOf(openingLetStmt) - 1)]

let writeOutput targetFile (txt: string) = task {
    use fs =
        FileInfo(targetFile)
            .Open(FileMode.Create, FileAccess.Write, FileShare.ReadWrite)

    use writer = new StreamWriter(fs)
    do! writer.WriteAsync(txt)
}

let writeDesugared (path) targetFile = task {
    try
        let file = FileInfo(path)
        //naïve
        if file.Length <> Interlocked.Exchange(&lastFileSize, file.Length) then
            use fs = file.Open(FileMode.Open, FileAccess.Read, FileShare.ReadWrite)
            use reader = new StreamReader(fs)
            let! content = reader.ReadToEndAsync()
            Console.WriteLine("Creating lowered output...")
            let! pp = TextCompiler.toLower content
            let opn = tryFindOpenStatements pp content
            do! writeOutput targetFile (opn + pp)
            Console.WriteLine("Wrote lowered output...")
        else
            Console.WriteLine("Ignored change, since file size did not change.")
    with exn ->
        Console.WriteLine(exn)
        Console.WriteLine("Failed write")
        do! Task.Delay(100)
        //meh, but only limited depth
        return ()

}


let asyncMain (dir: string) (targetFile: string) = backgroundTask {
    use cts = new CancellationTokenSource()

    Console.CancelKeyPress.Add(fun (a: ConsoleCancelEventArgs) ->
        a.Cancel <- true
        cts.Cancel())

    Console.WriteLine($"Watching folder {dir}")
    let files = DirectoryInfo(dir).GetFiles("*.fsx")
    Console.WriteLine($"Files in scope:")

    for file in files do
        Console.WriteLine(file.FullName)

    use watcher = new FileSystemWatcher(dir)
    watcher.NotifyFilter <- NotifyFilters.FileName ||| NotifyFilters.LastWrite
    watcher.EnableRaisingEvents <- true
    watcher.Filter <- "*.fsx"

    try
        while (not cts.Token.IsCancellationRequested) do
            let! args = waitOnChanged watcher cts.Token
            Console.WriteLine("Got change.")
            do! writeDesugared (args.FullPath) targetFile

    with :? OperationCanceledException ->
        ()

    Console.WriteLine("Exiting...")

    return ()
}

open Argu

type Arguments =
    | [<Mandatory; Unique>] Working_Directory of path: string
    | Target_Directory of path: string
    | FileName of string

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Working_Directory _ -> "Specify a working directory."
            | Target_Directory _ -> "Specify a directory where the lowered file should be output. Default: MyDocuments"
            | FileName _ -> "Name of the file with lowered output. Default: Desugared.fsx"


[<EntryPoint>]
let main args =
    let parser =
        ArgumentParser.Create<Arguments>(programName = "compexpr.exe").Parse(args)

    let dir = parser.GetResult(Working_Directory)

    let target =
        parser.GetResult(Target_Directory, Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments))

    Console.WriteLine($"Target: {target}")
    let targetFileName = parser.GetResult(FileName, "Desugared.fsx")
    let targetFile = Path.Combine(target, targetFileName)
    (asyncMain dir targetFile).GetAwaiter().GetResult()
    0
