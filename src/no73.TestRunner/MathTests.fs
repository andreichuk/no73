module MathTests

open no73.Core.Math
open Expecto

let PI = float32 (System.Math.PI)

module private LineSide =
    type VerificationCase =
        { id : string
          description : string option
          p1 : Point2f
          p2 : Point2f
          p : Point2f
          expectedSide : int }
    
    let verificationData = [ 
        {
            id = "2AA5FF0A-E85A-4FC3-BBCF-CFE67316DA74"
            description = Some("counter-clockwise: right side")
            p1 = { x = 0.0f; y = 0.0f }
            p2 = { x = 0.0f; y = 1.0f }
            p = { x = 1.0f; y = 0.0f }
            expectedSide = 1
        }
        {
            id = "0762A5D6-E206-480E-83E8-C2DB34BC7B5E"
            description = Some("counter-clockwise: left side (1)") 
            p1 = { x = 0.0f; y = 0.0f }
            p2 = { x = 0.0f; y = 1.0f }
            p = { x = -1.0f; y = 0.0f }
            expectedSide = -1
        }
        {
            id = "05393D4D-8BF1-4348-9182-559C92EED9E4"
            description = Some("counter-clockwise: left side (2)") 
            p1 = { x = -1.0f; y = 1.0f }
            p2 = { x = 0.0f; y = 0.0f }
            p = { x = 0.0f; y = 4.0f }
            expectedSide = -1
        }
        {
            id = "6EBB060D-861F-4D99-92B6-466C67E624CA"
            description = Some("counter-clockwise: left side (3)") 
            p1 = { x = 0.0f; y = 0.0f }
            p2 = { x = 1.0f; y = 1.0f }
            p = { x = 0.0f; y = 4.0f }
            expectedSide = -1
        }
        {
            id = "DAED7E64-6B3F-4668-9103-5A5E66A31593"
            description = Some("counter-clockwise: on the line") 
            p1 = { x = 0.0f; y = 0.0f }
            p2 = { x = 0.0f; y = 1.0f }
            p = { x = 0.0f; y = 4.0f }
            expectedSide = 0
        }
        {
            id = "99890C58-2571-4D7C-BCFA-1EF973F8D49F"
            description = Some("clockwise: right side") 
            p1 = { x = 0.0f; y = 1.0f }
            p2 = { x = 0.0f; y = 0.0f }
            p = { x = -1.0f; y = 1.0f }
            expectedSide = 1
        }
        { 
            id = "DC968F6C-6788-4ED8-8B88-271C88BC6925"
            description = Some("clockwise: left side")
            p1 = { x = 0.0f; y = 1.0f }
            p2 = { x = 0.0f; y = 0.0f }
            p = { x = 1.0f; y = 0.0f }
            expectedSide = -1
        }
        { 
            id = "8B7A0418-DC93-419D-BEC6-985E434C398A"
            description = Some("clockwise: on the line")
            p2 = { x = 0.0f; y = 4.0f }
            p1 = { x = 0.0f; y = 0.0f }
            p = { x = 0.0f; y = 1.0f }
            expectedSide = 0
        }
    ]

    let runner testCase =
        let side = lineSide testCase.p1 testCase.p2 testCase.p
        let description =
            match testCase.description with 
            | Some(text) -> text
            | None -> "line side"
        
        Expect.equal side testCase.expectedSide description

    let getTests () = 
        let createTestCase testCase = 
            test testCase.id { runner testCase }

        "line side", createTestCase, verificationData

module private PointInTriangle =
    type VerificationCase = {
        id : string
        description : string option
        a : Point2f
        b : Point2f
        c : Point2f
        p : Point2f
        inside : bool
    }
    
    let verificationData = [
        {
            id = "A3895AD1-6A73-422C-B4AF-EC09CB20FBA8"
            description = Some("inside")
            a = { x = 0.0f; y = 0.0f }
            b = { x = 0.0f; y = 100.0f }
            c = { x = 200.0f; y = 0.0f }
            p = { x = 1.0f; y = 3.0f }
            inside = true
        }
        {
            id = "2AC0E6C5-A3BB-4AEC-815F-8EFFC88DD51B"
            description = Some("outside") 
            a = { x = 0.0f; y = 0.0f }
            b = { x = 0.0f; y = 100.0f }
            c = { x = 200.0f; y = 0.0f }
            p = { x = -1.0f; y = 3.0f }
            inside = false
        }
        {
            id = "D7B8AA41-E064-4EE2-8221-D7E1F507AEB9"
            description = Some("vertex")
            a = { x = 0.0f; y = 0.0f }
            b = { x = 0.0f; y = 100.0f }
            c = { x = 200.0f; y = 0.0f }
            p = { x = 0.0f; y = 0.0f }
            inside = true
        }
        {
            id = "51CF543F-C471-4E50-9503-F39B73F7B3E3"
            description = Some("edge")
            a = { x = 0.0f; y = 0.0f }
            b = { x = 0.0f; y = 100.0f }
            c = { x = 200.0f; y = 0.0f }
            p = { x = 0.0f; y = 1.0f }
            inside = true
        }
    ]
    
    let runner testCase =
        let inside = pointInTriangle testCase.a testCase.b testCase.c testCase.p
        let description =
            match testCase.description with 
            | Some(text) -> text
            | None -> "inside triangle"
        
        Expect.equal inside testCase.inside description
    
    let getTests () = 
        let createTestCase testCase = 
            test testCase.id { runner testCase }
        
        "a point in a triangle", createTestCase, verificationData

module private PointsCounterClockwise =
    type VerificationCase = {
        id : string
        description : string option
        points : Point2f list
        counterClockwise : bool
    }
    
    let verificationData = [
        {
            id = "8F452941-5282-43D2-BB14-F1F16B7A2E7B"
            description = Some("3 points: counter-clockwise")
            points = [
                { x = 0.0f; y = 0.0f }
                { x = 1.0f; y = 0.0f }
                { x = 0.0f; y = 1.0f }
            ]
            counterClockwise = true
        }
        {
            id = "9017C8B3-DA5C-4D6F-A1AC-308CC6033176"
            description = Some("3 points: clockwise") 
            points = [
                { x = 0.0f; y = 1.0f }
                { x = 1.0f; y = 0.0f }
                { x = 0.0f; y = 0.0f }
            ]
            counterClockwise = false
        }
        {
            id = "D75C6698-17C8-4A4B-9633-559CED4C061F"
            description = Some( "4 points: counter-clockwise") 
            points = [
                { x = -1.0f; y = 0.0f }
                { x = 0.0f; y = -1.0f }
                { x = 1.0f; y = 0.0f }
                { x = 0.0f; y = 1.0f }
            ]
            counterClockwise = true
        }
        {
            id = "299A0704-1BA7-420B-932A-7109A6A2D331"
            description = Some("4 points: clockwise")
            points = [
                { x = 0.0f; y = 1.0f }
                { x = 1.0f; y = 0.0f }
                { x = 0.0f; y = -1.0f }
                { x = -1.0f; y = 0.0f }
            ]
            counterClockwise = false
        }
    ]
        
    let runner testCase =
        let direction = counterClockwise testCase.points
        let description =
            match testCase.description with 
            | Some(text) -> text
            | None -> "counter-clockwise"
        
        Expect.equal direction testCase.counterClockwise description

    let getTests () = 
        let createTestCase testCase = 
            test testCase.id { runner testCase }
        
        "points counter-clockwise", createTestCase, verificationData

module private ValueBetween =
    type VerificationCase = {
        id : string
        description : string option
        p1 : float32
        p : float32
        p2 : float32
        between : bool
    }
    
    let verificationData = [
        {
            id = "695F1E04-2569-4786-9960-7361155A20B7"
            description = Some("between (ordinal order)")
            p1 = 1.0f
            p = 2.0f
            p2 = 3.0f
            between = true
        }
        { 
            id = "8A236C34-EC11-480E-9BC4-36480823AD47"
            description = Some("between (reverse order)")
            p1 = 3.0f
            p = 2.0f
            p2 = 1.0f
            between = true
         }
        {
            id = "B116C601-E642-43F6-AAA3-EAE765238817"
            description = Some("not between (ordinal order)")  
            p1 = 1.0f
            p = 4.0f
            p2 = 3.0f
            between = false
        }
        { 
            id = "D3865A89-1062-4FC0-8ABF-7A8F0C635A65"
            description = Some("not between (reverse order)")
            p1 = 3.0f
            p = 4.0f
            p2 = 1.0f
            between = false
        }
        {
            id = "789CC6CB-5DB4-4DCD-8C84-7175C32F3C30"
            description = Some("between: same values") 
            p1 = 1.0f
            p = 1.0f
            p2 = 1.0f
            between = true
        }
    ]
        
    let runner testCase =
        let positionBetween = between testCase.p1 testCase.p testCase.p2
        let description =
            match testCase.description with 
            | Some(text) -> text
            | None -> "value between"
    
        Expect.equal positionBetween testCase.between description

    let getTests () = 
        let createTestCase testCase = 
            test testCase.id { runner testCase }
        "value between", createTestCase, verificationData

module private CollinearPoints =
    type VerificationCase = {
        id : string
        description : string option
        p1 : Point2f
        p2 : Point2f
        p3 : Point2f
        result : bool
    }
    
    let verificationData = [
        {
            id = "5DA389CA-8C14-4898-9BFC-C47A514992D7"
            description = Some("collinear points: horizontal line")
            p1 = { x = 1.0f; y = 0.0f }
            p2 = { x = 2.0f; y = 0.0f }
            p3 = { x = 3.0f; y = 0.0f }
            result = true
        }
        {
            id = "C06088B9-4B27-4ED9-A4CF-43F8BDCE252E"
            description = Some("collinear points: vertical line")
            p1 = { x = 0.0f; y = 1.0f }
            p2 = { x = 0.0f; y = 2.0f }
            p3 = { x = 0.0f; y = 3.0f }
            result = true
        }
        { 
            id = "41C333A4-CE7D-40A3-9CCA-18623794CF22"
            description = Some("collinear points: 45-degrees line")
            p1 = { x = 1.0f; y = 1.0f }
            p2 = { x = 2.0f; y = 2.0f }
            p3 = { x = 3.0f; y = 3.0f }
            result = true
        }
        { 
            id = "D4BD5AEE-2C94-4C07-A42C-DC1709503884"
            description = Some("not collinear points")
            p1 = { x = 1.0f; y = 1.0f }
            p2 = { x = 12.0f; y = 10.0f }
            p3 = { x = 3.0f; y = 3.0f }
            result = false
        }
    ]

    let runner testCase =
        let result = collinearPoints testCase.p1 testCase.p2 testCase.p3
        let description =
            match testCase.description with 
            | Some(text) -> text
            | None -> "collinear"
    
        Expect.equal result testCase.result description

    let getTests () = 
        let createTestCase testCase = 
            test testCase.id { runner testCase }
        
        "collinear points", createTestCase, verificationData

module private PointOnTheLine =
    type VerificationCase = {
        id : string
        description : string option
        point : Point2f
        segment : LineSegment
        result : bool
    }
    
    let verificationData = [
        {
            id = "A0D534AA-B5BE-4F5E-9B36-95617EAE0C28"
            description = Some("on the line: horizontal line")
            point = { x = 1.0f; y = 0.0f }
            segment = {
                p0 = { x = 0.0f; y = 0.0f }
                p1 = { x = 2.0f; y = 0.0f }
            }
            result = true 
        }
        { 
            id = "075CF981-3525-4856-B36F-33A32406AC45"
            description = Some("on the line: vertical line")
            point = { x = 0.0f; y = 1.0f }
            segment = {
                p0 = { x = 0.0f; y = 0.0f }
                p1 = { x = 0.0f; y = 2.0f }
            }
            result = true
        }
        { 
            id = "73D3649F-2206-437E-A542-3DF64457A1A6"
            description = Some("on the line: 45-degrees line")
            point = { x = 1.0f; y = 1.0f }
            segment = {
                p0 = { x = 0.0f; y = 0.0f }
                p1 = { x = 2.0f; y = 2.0f }
            }
            result = true
        }
        { 
            id = "391FA0AF-8814-4EB5-A658-065FE0182978"
            description = Some("on the line: at the segment's end")
            point = { x = 0.0f; y = 0.0f }
            segment = {
                p0 = { x = 0.0f; y = 0.0f }
                p1 = { x = 2.0f; y = 2.0f }
            }
            result = true
        }
        { 
            id = "7942726E-81DC-430B-B5C5-BA46A130AAC3"
            description = Some("not on the line")
            point = { x = 1.0f; y = 1.0f }
            segment = {
                p0 = { x = 10.0f; y = 15.0f }
                p1 = { x = 40.0f; y = 42.0f }
            }
            result = false
        }
        { 
            id = "FCA3234A-0088-40D2-A4B7-3CFF0C84E1E3"
            description = Some("on the line: all points are the same")
            point = { x = 1.0f; y = 1.0f }
            segment = {
                p0 = { x = 1.0f; y = 1.0f }
                p1 = { x = 1.0f; y = 1.0f }
            }
            result = true
        }
    ]
        
    let runner testCase =
        let result = pointOnLineSegment testCase.point testCase.segment
        let description =
            match testCase.description with 
            | Some(text) -> text
            | None -> "on the line segment"
    
        Expect.equal result testCase.result description

    let getTests () = 
        let createTestCase testCase = 
            test testCase.id { runner testCase }
        
        "points on a line segment", createTestCase, verificationData
        
module LineSegmentsIntersection =
    type VerificationCase = {
        id : string
        description : string option
        segment1 : LineSegment
        segment2 : LineSegment
        result : Point2f option
    }
    
    let verificationData = [
        {
            id = "FFDB1B84-C731-4948-90D6-87C9912DA849"
            description = Some("intersection (not collinear)") 
            segment1 = {
                p0 = { x = 0.0f; y = 0.0f }
                p1 = { x = 2.0f; y = 2.0f }
            }
            segment2 = {
                p0 = { x = 0.0f; y = 2.0f }
                p1 = { x = 2.0f; y = 0.0f }
            }
            result = Some({ x = 1.0f; y = 1.0f })
        }
        { 
            id = "6B960F1B-6A7D-4BF6-9E56-FCE4CBEC6AF5"
            description = Some("no intersection")
            segment1 = {
                p0 = { x = 0.0f; y = 0.0f }
                p1 = { x = 2.0f; y = 2.0f }
            }
            segment2 = {
                p0 = { x = -10.0f; y = -2.0f }
                p1 = { x = -2.0f; y = -10.0f }
            }
            result = None
        }
        {
            id = "F626D8FA-8C2E-4D28-9547-1F73B6856A53"
            description = Some("intersection (share 1 point)") 
            segment1 = {
                p0 = { x = 0.0f; y = 0.0f }
                p1 = { x = 2.0f; y = 2.0f }
            }
            segment2 = {
                p0 = { x = 2.0f; y = 2.0f }
                p1 = { x = 2.0f; y = 4.0f }
            }
            result = Some({ x = 2.0f; y = 2.0f })
        }
        {
            id = "7BC968A0-1C44-45CB-9D92-460860DE78E6"
            description = Some("intersection (collinear, share a segment)") 
            segment1 = {
                p0 = { x = 0.0f; y = 0.0f }
                p1 = { x = 2.0f; y = 2.0f }
            }
            segment2 = {
                p0 = { x = 1.0f; y = 1.0f }
                p1 = { x = 3.0f; y = 3.0f }
            }
            result = None
        }
        {
            id = "214D87E5-8B37-4C2A-9225-B7CF6BC0F612"
            description = Some("no intersection (collinear, no overlapping)") 
            segment1 = {
                p0 = { x = 0.0f; y = 0.0f }
                p1 = { x = 2.0f; y = 2.0f }
            }
            segment2 = {
                p0 = { x = 3.0f; y = 3.0f }
                p1 = { x = 4.0f; y = 4.0f }
            }
            result = None
        }
        {
            id = "F59CB7C7-6A74-46A8-BD93-3DD90C1CA9D1"
            description = Some("intersection (parallel)") 
            segment1 = {
                p0 = { x = 0.0f; y = 0.0f }
                p1 = { x = 2.0f; y = 2.0f }
            }
            segment2 = {
                p0 = { x = 2.0f; y = 0.0f }
                p1 = { x = 4.0f; y = 2.0f }
            }
            result = None
        }
    ]
        
    let runner testCase =
        let result = lineSegmentsIntersect testCase.segment1 testCase.segment2
        let description =
            match testCase.description with 
            | Some(text) -> text
            | None -> "line segments intersection"
    
        Expect.equal result testCase.result description

    let getTests () = 
        let createTestCase testCase = 
            test testCase.id { runner testCase }
        
        "line segments intersection", createTestCase, verificationData

module private LineSegmentsAngle =
    type VerificationCase = {
        id : string
        description : string option
        segment1 : LineSegment
        segment2 : LineSegment
        result : float32
    }
    
    let verificationData = [
        {
            id = "DAAF7988-F7CA-405F-9C23-C1DD0A7DAEB4"
            description = Some("0 degrees")
            segment1 = {
                p0 = { x = 0.0f; y = 0.0f }
                p1 = { x = 1.0f; y = 0.0f }
            }
            segment2 = {
                p0 = { x = 0.0f; y = 0.0f }
                p1 = { x = 2.0f; y = 0.0f }
            }
            result = 0.0f
        }
        {
            id = "68DDC175-3795-46C4-A8E4-7A98D62AA7AD"
            description = Some("45 degrees") 
            segment1 = {
                p0 = { x = 0.0f; y = 0.0f }
                p1 = { x = 1.0f; y = 0.0f }
            }
            segment2 = {
                p0 = { x = 0.0f; y = 0.0f }
                p1 = { x = 1.0f; y = -1.0f }
            }
            result = PI / 4.0f
        }
        {
            id = "C86F76AB-B2AA-4FF2-BD86-02418ED21C06"
            description = Some("90 degrees") 
            segment1 = {
                p0 = { x = 0.0f; y = 0.0f }
                p1 = { x = 1.0f; y = 0.0f }
            }
            segment2 = {
                p0 = { x = 0.0f; y = 0.0f }
                p1 = { x = 0.0f; y = -1.0f }
            }
            result = PI / 2.0f
        }
        {
            id = "B159E239-F4CD-4818-9091-00FE52754FBD"
            description = Some("180 degrees") 
            segment1 = {
                p0 = { x = 0.0f; y = 0.0f }
                p1 = { x = 1.0f; y = 0.0f }
            }
            segment2 = {
                p0 = { x = 0.0f; y = 0.0f }
                p1 = { x = -1.0f; y = 0.0f }
            }
            result = PI
        }
        {
            id = "7840AC8F-0694-4D0B-8B98-4583F14DA2D6"
            description = Some("270 degrees")
            segment1 = {
                p0 = { x = 0.0f; y = 0.0f }
                p1 = { x = 1.0f; y = 0.0f }
            }
            segment2 = {
                p0 = { x = 0.0f; y = 0.0f }
                p1 = { x = 0.0f; y = 1.0f }
            }
            result = PI * 1.5f
        }
    ]
      
    let runner testCase =
        let result = lineSegmentsAngle testCase.segment1 testCase.segment2
        let description =
            match testCase.description with 
            | Some(text) -> text
            | None -> "angle between segments"
    
        Expect.equal result testCase.result description

    let getTests () = 
        let createTestCase testCase = 
            test testCase.id { runner testCase }

        "an angle between segments", createTestCase, verificationData
        
let getTests() =
    seq {
        yield Util.testList LineSide.getTests 
        yield Util.testList PointInTriangle.getTests
        yield Util.testList PointsCounterClockwise.getTests
        yield Util.testList ValueBetween.getTests
        yield Util.testList CollinearPoints.getTests
        yield Util.testList PointOnTheLine.getTests
        yield Util.testList LineSegmentsIntersection.getTests
        yield Util.testList LineSegmentsAngle.getTests
    }