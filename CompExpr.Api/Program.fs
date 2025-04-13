namespace CompExpr.Api

#nowarn "20"

open Microsoft.AspNetCore.Http.Json
open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.Hosting

module Program =
    open Microsoft.AspNetCore.Http
    open CompExpr
    open System
    open System.Text

    let exitCode = 0

    [<EntryPoint>]
    let main args =
        //warmup
        try
            let _pp = TextCompiler.toLower("let a = ()").GetAwaiter().GetResult()
            ()
        with exn ->
            Console.WriteLine("Exception during warmup")
            Console.WriteLine(exn.Message)

        let builder = WebApplication.CreateBuilder(args)
        let app = builder.Build()

        app
            .UseFileServer()
            .UseRouting()
            .UseEndpoints(fun a ->
                a.MapPost(
                    "lowered",
                    RequestDelegate(fun ctx -> task {
                        try
                            let! x = ctx.Request.ReadFromJsonAsync<{| content: string |}>()
                            let! lowered = TextCompiler.toLower (x.content)
                            ctx.Response.Headers.ContentType <- "text/plain"
                            do! ctx.Response.WriteAsJsonAsync({| data = lowered |})
                        with exn ->
                            ctx.Response.Headers.ContentType <- "text/plain"
                            do! ctx.Response.WriteAsJsonAsync({| error = exn.ToString() |})

                        return ()
                    })
                )
                |> ignore)


        app.Run()

        exitCode
