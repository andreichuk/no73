module TriangulationTests

open no73.Core.Math
open Expecto

module private EarTip =
    type VerificationCase = {
        id : string
        description : string option
        v0 : Point2f
        v1 : Point2f
        v2 : Point2f
        vertices : Point2f list
        result : bool
    }
    
    let verificationData = [
        {
            id = "2187FDF5-0DC2-40C2-9959-6CAC3590846C"
            description = Some("4 vertices: ear tip")
            v0 = { x = -1.0f; y = 0.0f }
            v1 = { x = 0.0f; y = -1.0f }
            v2 = { x = 1.0f; y = 0.0f }
            vertices = [
                { x = 0.0f; y = 1.0f }
            ]
            result = true
        }
        {
            id = "BBC7E3E8-5D42-4CD7-B975-10A782248054"
            description = Some("5 vertices: ear tip")
            v0 = { x = -1.0f; y = 0.0f }
            v1 = { x = 0.0f; y = -1.0f }
            v2 = { x = 1.0f; y = 0.0f }
            vertices = [
                { x = 0.0f; y = 1.0f }
                { x = -1.0f; y = 1.0f }
            ]
            result = true
        }
        {
            id = "D7A50A82-FB02-4155-9013-A8F1B8CE4F9F"
            description = Some("5 vertices: not ear tip") 
            v0 = { x = 3.0f; y = 0.0f }
            v1 = { x = 5.0f; y = 2.0f }
            v2 = { x = 2.0f; y = 4.0f }
            vertices = [
                { x = 3.0f; y = 2.0f }
                { x = 1.0f; y = 1.0f }
            ]
            result = false
        }
    ]
    
    let runner testCase =
        let result = Triangulation.TriangulationEngine.vertexIsEarTip testCase.v0 testCase.v1 testCase.v2 testCase.vertices
        let description =
            match testCase.description with 
            | Some(text) -> text
            | None -> "the vertex is an ear tip"
    
        Expect.equal result testCase.result description

    let getTests () = 
        let createTestCase testCase = 
            test testCase.id { runner testCase }

        "an ear tip", createTestCase, verificationData

module private Triangle =
    type VerificationCase = {
        id : string
        description : string option
        polygon : Triangulation.RawPolygon
    }
    
    let verificationData = [
        {
            id = "43FC4944-8E1E-40A9-951C-B313BF4A43C2"
            description = Some("4 vertices: 2 triangles")
            polygon = {
                Triangulation.RawPolygon.points = [
                    { x = -1.0f; y = 0.0f }
                    { x = 0.0f; y = -1.0f }
                    { x = 1.0f; y = 0.0f }
                    { x = 0.0f; y = 1.0f } 
                ]
            }
        }
        { 
           id = "97FBE776-5E52-4516-B81C-C18C39EC6BFA"
           description = Some("5 vertices: 3 triangles")
           polygon = {
               Triangulation.RawPolygon.points = [
                   { x = -2.0f; y = 0.0f }
                   { x = -1.0f; y = -1.0f }
                   { x = 1.0f; y = -1.0f }
                   { x = 2.0f; y = 0.0f }
                   { x = 0.0f; y = 1.0f }
               ]
           }
        }
        {
            id = "48FC4030-9476-4E14-A3F1-DC2AE11D9F36"
            description = Some("10 vertices (with a 'hole'): 8 triangles") 
            polygon = {
                Triangulation.RawPolygon.points = [
                    { x = 1.0f; y = 1.0f }
                    { x = 4.0f; y = 1.0f }
                    { x = 4.0f; y = 4.0f }
                    { x = 1.0f; y = 4.0f }
                    { x = 1.0f; y = 1.0f }
                    { x = 2.0f; y = 2.0f }
                    { x = 2.0f; y = 3.0f }
                    { x = 3.0f; y = 3.0f }
                    { x = 3.0f; y = 2.0f }
                    { x = 2.0f; y = 2.0f }
                ]
            }
        }
        {
            id = "2DC4D581-DEAF-4B86-9019-5C2F47CC5DD8"
            description = Some("5 vertices : 7 triangles")
            polygon = {
                Triangulation.RawPolygon.points = [
                    { x = 5.0f; y = 4.0f }
                    { x = 2.0f; y = 4.0f }
                    { x = 5.0f; y = 1.0f }
                    { x = 8.0f; y = 4.0f }
                    { x = 5.0f; y = 7.0f }
                ]
            }
        }
        {
            id = "32184D53-BA46-4817-95C5-4E7A229603D0"
            description = Some("6 vertices (with a duplicated vertice/shared edge): 8 triangles")
            polygon = {
                Triangulation.RawPolygon.points = [
                    { x = 2.0f; y = 4.0f }
                    { x = 5.0f; y = 4.0f }
                    { x = 2.0f; y = 3.95f }
                    { x = 5.0f; y = 1.0f }
                    { x = 8.0f; y = 4.0f }
                    { x = 5.0f; y = 7.0f }
                ]
            }
        }
    ]

    let verticeInTriangle point triangle = triangle.v0 = point || triangle.v1 = point || triangle.v2 = point
    let verticeInTriangles triangles point = List.exists (verticeInTriangle point) triangles
    let triangleCount vertices = List.length vertices - 2
    
    let runner testCase =
        let polygon = testCase.polygon
        let triangles = Triangulation.TriangulationEngine.triangulate polygon []
        let expectedCount = triangleCount polygon.points
        let actualCount = List.length triangles
        let allInTriangles = List.forall (verticeInTriangles triangles) polygon.points
        
        let description =
            match testCase.description with 
            | Some(text) -> text
            | None -> "triangles"
        
        Expect.equal actualCount expectedCount (description + " --> triangles count")
        Expect.isTrue allInTriangles (description + " --> all vertices used")
    
    let getTests () =
        let createTestCase testCase = 
            test testCase.id { runner testCase }

        "a triangle", createTestCase, verificationData

module private Hole =
    type VerificationData = {
        id : string
        description : string option
        polygon : Triangulation.RawPolygon
        holes : Triangulation.RawPolygon list
    }
    
    let verificationData = [
        {
            id = "024B3E87-F028-4AA1-9645-A24F1617522D"
            description = None
            polygon = {
                Triangulation.RawPolygon.points = [
                    { x = 1.0f; y = 4.0f }
                    { x = 4.0f; y = 1.0f }
                    { x = 7.0f; y = 4.0f }
                    { x = 4.0f; y = 7.0f }
                ]
            }
            holes = [
                { 
                    Triangulation.RawPolygon.points = [
                        { x = 3.0f; y = 4.0f }
                        { x = 4.0f; y = 5.0f }
                        { x = 5.0f; y = 3.0f }
                      ]
                }
            ]
        }
        {
            id = "209CA189-C2D1-496B-8870-4EE416E82F9B"
            description = None
            polygon = {
                Triangulation.RawPolygon.points = [
                    { x = 1.0f; y = 1.0f }
                    { x = 10.0f; y = 1.0f }
                    { x = 11.0f; y = 4.0f }
                    { x = 12.0f; y = 1.0f }
                    { x = 14.0f; y = 1.0f }
                    { x = 14.0f; y = 7.0f }
                    { x = 1.0f; y = 7.0f }
                ]
            }
            holes = [
                {
                    Triangulation.RawPolygon.points = [
                        { x = 3.0f; y = 3.0f }
                        { x = 4.0f; y = 6.0f }
                        { x = 6.0f; y = 5.0f }
                    ]
                }
            ]
        }
        {
            id = "3A5D46A5-AD39-4E2D-B5A9-C60EB611732E"
            description = None
            polygon = {
                Triangulation.RawPolygon.points = [
                    { x = 1.0f; y = 1.0f }
                    { x = 8.0f; y = 4.0f }
                    { x = 1.0f; y = 8.0f }
                ]
            }
            holes = [
                {
                    Triangulation.RawPolygon.points = [
                        { x = 2.0f; y = 3.0f }
                        { x = 3.0f; y = 5.0f } 
                        { x = 5.0f; y = 4.0f }
                    ]
                }
            ]
        }
        {
            id = "4AA0EA27-BAFA-4959-A97B-E6389FFC60DA"
            description = None 
            polygon = {
                Triangulation.RawPolygon.points = [
                    { x = 1.0f; y = 1.0f }
                    { x = 12.0f; y = 1.0f }
                    { x = 14.0f; y = 7.0f }
                    { x = 16.0f; y = 1.0f }
                    { x = 17.0f; y = 1.0f }
                    { x = 18.0f; y = 6.0f }
                    { x = 19.0f; y = 1.0f }
                    { x = 20.0f; y = 1.0f }
                    { x = 21.0f; y = 8.0f }
                    { x = 22.0f; y = 1.0f }
                    { x = 24.0f; y = 1.0f }
                    { x = 24.0f; y = 11.0f }
                    { x = 1.0f; y = 11.0f }
                ]
            }
            holes = [
                {
                    Triangulation.RawPolygon.points = [
                        { x = 2.0f; y = 4.0f }
                        { x = 3.0f; y = 9.0f }
                        { x = 4.0f; y = 10.0f }
                    ]
                }
            ]
        }
    ]

    let runner testCase =
        let triangleCount =
            Triangulation.TriangulationEngine.triangulate testCase.polygon testCase.holes
            |> List.length
        let atLeast1Triangle = triangleCount > 0
        Expect.isTrue atLeast1Triangle "at least 1 triangle"
    
    let getTests () =
        let createTestCase testCase = 
            test testCase.id { runner testCase }

        "holes", createTestCase, verificationData

let getTests () =
    seq {
        yield Util.testList EarTip.getTests
        yield Util.testList Triangle.getTests
        yield Util.testList Hole.getTests
    }