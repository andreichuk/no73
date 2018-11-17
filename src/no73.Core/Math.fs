namespace no73.Core

module Math =
    [<Struct>]
    type Point2f =
        { x : float32
          y : float32 }
    
    [<Struct>]
    type LineSegment =
        { p0 : Point2f
          p1 : Point2f }
    
    [<Struct>]
    type Triangle =
        { v0 : Point2f
          v1 : Point2f
          v2 : Point2f }
    
    let approx (a : float32) (b : float32) = System.Math.Abs(a - b) < 0.0001f
    let greaterOrEqual (a : float32) (b : float32) = a > b || (approx a b)
    let lessOrEqual (a : float32) (b : float32) = a < b || (approx a b)
    
    let lineLength p1 p2 =
        let xDistance = p2.x - p1.x
        let yDistance = p2.y - p1.y
        xDistance ** 2.0f + yDistance ** 2.0f
        |> float
        |> System.Math.Sqrt
        |> float32
    
    let lineSide p1 p2 p =
        (// d = (x − x1)(y2 − y1) − (y − y1)(x2 − x1)
         // for counter-clockwise
         // -1 = left side
         //  0 = on the line
         // +1 = right side
         p.x - p1.x) * (p2.y - p1.y) - (p.y - p1.y) * (p2.x - p1.x) |> System.Math.Sign
    
    // using barycentric coordinates
    let pointInTriangle a b c p =
        let area = 0.5f * (a.y * (c.x - b.x) - b.y * c.x + a.x * (b.y - c.y) + b.x * c.y)
        let s = (a.y * c.x - a.x * c.y + (c.y - a.y) * p.x + (a.x - c.x) * p.y) / (2.0f * area)
        let t = (a.x * b.y - a.y * b.x + (a.y - b.y) * p.x + (b.x - a.x) * p.y) / (2.0f * area)
        s >= 0.0f && t >= 0.0f && (s + t) <= 1.0f
    
    let counterClockwise points =
        let calc p1 p2 = (p2.x - p1.x) * (p2.y + p1.y)
        
        let rec loop pts prevP sum =
            match pts with
            | [] -> prevP, sum
            | head :: tail -> 
                let newSum = sum + calc prevP head
                loop tail head newSum
        match points with
        | [] -> false
        | p1 :: [] -> false
        | p1 :: p2 :: [] -> false
        | p1 :: tail -> 
            let lastP, sum = loop tail p1 0.0f
            let sum = sum + calc lastP p1
            sum < 0.0f
    
    // 3 points lie on the same line
    let collinearPoints a b c = (b.x - a.x) * (c.y - a.y) = (c.x - a.x) * (b.y - a.y)
    // detects whether value v1 lies between v0 & v2
    let inline between v0 v1 v2 = (v0 <= v1 && v1 <= v2) || (v2 <= v1 && v1 <= v0)
    
    // detects whether a point lies on the line segment
    let pointOnLineSegment point seg =
        let collinear = collinearPoints point seg.p0 seg.p1
        
        let pointBetween =
            if seg.p0.x <> seg.p1.x then between seg.p0.x point.x seg.p1.x
            else between seg.p0.y point.y seg.p1.y
        
        let onLine = collinear && pointBetween
        onLine
    
    let lineSegmentsIntersect seg1 seg2 =
        let p0x = seg1.p0.x
        let p0y = seg1.p0.y
        let p1x = seg1.p1.x
        let p1y = seg1.p1.y
        let p2x = seg2.p0.x
        let p2y = seg2.p0.y
        let p3x = seg2.p1.x
        let p3y = seg2.p1.y
        let s1x = p1x - p0x
        let s1y = p1y - p0y
        let s2x = p3x - p2x
        let s2y = p3y - p2y
        let div = -s2x * s1y + s1x * s2y
        if div = 0.0f then None
        else 
            let s' = -s1y * (p0x - p2x) + s1x * (p0y - p2y)
            let t' = s2x * (p0y - p2y) - s2y * (p0x - p2x)
            let sSign = System.Math.Sign(s')
            let tSign = System.Math.Sign(t')
            let divSign = System.Math.Sign(div)
            // div can be very large so s and/or t will be zero. in this case it's important to know
            // whether s or t is less than zero in order to get a correct result (intersects or not)
            let s = s' / div
            let t = t' / div
            if (sSign * divSign >= 0 && tSign * divSign >= 0 && s >= 0.0f && s <= 1.0f && t >= 0.0f && t <= 1.0f) then 
                Some { x = p0x + (t * s1x)
                       y = p0y + (t * s1y) }
            else None
    
    let lineSegmentsAngle seg1 seg2 =
        let theta1 = System.Math.Atan2(float (seg1.p0.y - seg1.p1.y), float (seg1.p0.x - seg1.p1.x))
        let theta2 = System.Math.Atan2(float (seg2.p0.y - seg2.p1.y), float (seg2.p0.x - seg2.p1.x))
        let angle = float32 (System.Math.Abs(theta1 - theta2))
        angle
