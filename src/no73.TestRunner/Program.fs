module Program

open System
open Expecto
open no73.Core.Math
open no73.Core

let startTestRun() =
    let config = defaultConfig
    let tests =
        seq { 
            yield! MathTests.getTests()
            yield! TriangulationTests.getTests()
            yield AStarTests.getTests()
        }

    tests
    |> Seq.map (runTests config)
    |> Seq.iter (fun _ -> ())

[<EntryPoint>]
let main argv =
    startTestRun()
    Console.WriteLine("finished")
    Console.ReadLine() |> ignore
    0
