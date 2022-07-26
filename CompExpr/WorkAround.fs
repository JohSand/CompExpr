namespace System

type IAsyncDisposable =
    abstract member DisposeAsync: unit -> System.Threading.Tasks.ValueTask

