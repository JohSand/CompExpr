module Tests

open System
open Xunit
open CompExpr

[<Fact>]
let ``My test`` () =
    async {
        let fsharp =
                "\
module A
open CompExpr.Program

let e() = 
    scenario {
        try
            do! generateScenario()
        with _e ->
            do ()
    }"
        let expected = "\
let e =
    (fun (builder: ScenarioStateBuilder)
        builder.Run(
            builder.Delay (fun ()
                builder.TryWith(
                    builder.Delay (fun ()
                        builder.Bind(CompExpr.Program.generateScenario (), (fun (_arg1: unit)builder.Return()),
                    fun (_arg2: exn)
                        let _e = _arg2
                        ()
                        builder.Zero()


        scenario
"

        let! result = Mapper.toLower fsharp
        Assert.Equal(expected, result)
    }