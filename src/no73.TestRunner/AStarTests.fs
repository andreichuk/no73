module AStarTests

open no73.Core
open Expecto

type private Cell = {
    id : int
    point : float32 * float32
    adjacent : int Set
}

type private VerificationCase = {
    id : string
    description : string option
    start : int
    goal : int
    path : int list option
}

type private TestCase = {
    cells : Cell list
    verificationData : VerificationCase seq
}

let private idGetter cell = cell.id

let private testCases =
    seq { 
        yield {
            cells = [
                {
                    id = 1
                    point = (1.0f, 1.0f)
                    adjacent = [ 2; 3; 4 ] |> Set.ofList
                }
                {
                    id = 2
                    point = (1.0f, 4.0f)
                    adjacent = [ 1; 4 ] |> Set.ofList
                }
                {
                    id = 3
                    point = (4.0f, 1.0f)
                    adjacent = [ 1; 5; 6 ] |> Set.ofList
                }
                {
                    id = 4
                    point = (6.0f, 6.0f)
                    adjacent = [ 1; 2; 5 ] |> Set.ofList
                }
                {
                    id = 5
                    point = (7.0f, 5.0f)
                    adjacent = [ 3; 4; 6; 7 ] |> Set.ofList
                }
                {
                    id = 6
                    point = (8.0f, 1.0f)
                    adjacent = [ 3; 5; 7 ] |> Set.ofList
                }
                {
                    id = 7
                    point = (11.0f, 3.0f)
                    adjacent = [ 5; 6 ] |> Set.ofList
                }
            ]
            verificationData =
                seq { 
                    yield { 
                        id = "FD936608-B9A1-4459-A1D6-702015450F87"
                        description = None
                        start = 1
                        goal = 7
                        path = Some([ 1; 3; 6; 7 ]) }
                    }
                }
    }

let private run' cells case =
    let start = case.start
    let goal = case.goal
    
    let adjacentGetter id =
        let cell = Map.find id cells
        cell.adjacent
    
    let distance id1 id2 =
        let { point = (x1, y1) } = Map.find id1 cells
        let { point = (x2, y2) } = Map.find id2 cells
        System.MathF.Sqrt((x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2))
    
    let heuristic id = distance id goal
    
    let cmd = {
        AStar.Cmd.start = start
        AStar.Cmd.goal = goal
        AStar.Cmd.adjacentGetter = adjacentGetter
        AStar.Cmd.distance = distance
        AStar.Cmd.heuristic = heuristic
    }
    
    let path = AStar.find cmd
    
    let description =
        match case.description with
        | Some(text) -> text
        | None -> "path"
    Expect.equal path case.path description

let getTests() =
    let createExpectoCase cells testCase = test testCase.id { run' cells testCase }

    let runTestCase testCase =
        let cells =
            testCase.cells
            |> List.map (fun c -> c.id, c)
            |> Map.ofList

        let createExpectoCase' = createExpectoCase cells
        testCase.verificationData |> Seq.map createExpectoCase'
    
    let tests = testCases |> Seq.collect runTestCase |> List.ofSeq
    testList "A*/pathfinding" tests