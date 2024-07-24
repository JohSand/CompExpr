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

        let _pp = TextCompiler.toLower "let a = ()" |> Async.RunSynchronously

        let builder = WebApplication.CreateBuilder(args)
        let app = builder.Build()
        app
            .UseFileServer()
            .UseRouting()
            .UseEndpoints(fun a ->
                a.MapPost(
                    "lowered",
                    RequestDelegate(fun ctx ->
                        task {
                            //let query = ctx.Request.Query["code"]
                            let! x = ctx.Request.ReadFromJsonAsync<{| content: string |}>()
                            //let decoded = Encoding.UTF8.GetString(Convert.FromBase64String(query))
                            let! lowered = TextCompiler.toLower (x.content)
                            ctx.Response.Headers.ContentType <- "text/plain"
                            do! ctx.Response.WriteAsJsonAsync({| data = lowered |})
                            return ()
                        })
                )
                |> ignore)


        app.Run()

        exitCode
