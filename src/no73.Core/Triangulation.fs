module Triangulation

open no73.Core
open no73.Core.Math

type RawPolygon =
    { points : Point2f list }

module PolygonEngine =
    let vertexIsConvex pPrev p pNext =
        // The order of vertices is counter-clockwise.
        // A convex vertex is one for which the interior angle is smaller than 180 degrees.
        // For determining the angle it's enough to know what side p lies relative to the line pPrev-pNext on
        let side = Math.lineSide pPrev pNext p
        let convex = side = 1
        convex
    
    let checkIfConvex vertices vKey (vPrevKey, vNextKey) =
        let p = Map.find vKey vertices
        let pPrev = Map.find vPrevKey vertices
        let pNext = Map.find vNextKey vertices
        vertexIsConvex pPrev p pNext
    
    let getAdjacent vertices =
        let verticeKeySeq =
            vertices
            |> Map.toSeq
            |> Seq.map fst
        
        let firstVerticeKey = verticeKeySeq |> Seq.min
        let lastVerticeKey = verticeKeySeq |> Seq.max
        
        let getAdjacent state key _ =
            let adjacent =
                match key with
                | _ when key = firstVerticeKey -> (lastVerticeKey, firstVerticeKey + 1)
                | _ when key = lastVerticeKey -> (lastVerticeKey - 1, firstVerticeKey)
                | _ -> (key - 1, key + 1)
            Map.add key adjacent state
        
        let adjacent = Map.fold getAdjacent Map.empty vertices
        adjacent
    
    let checkPoints vertices adjacent reflex v =
        let vPrev, vNext = Map.find v adjacent
        let pPrev = Map.find vPrev vertices
        let p = Map.find v vertices
        let pNext = Map.find vNext vertices
        
        let toCheck =
            Set.filter (fun v0 -> v0 <> v && v0 <> vPrev && v0 <> vNext) reflex
            |> List.ofSeq
            |> List.map (fun v0 -> Map.find v0 vertices)
        
        let pointInTriangle' = Math.pointInTriangle pPrev p pNext
        let earTip = List.exists pointInTriangle' toCheck |> not
        earTip
    
    let getConvexReflexVertices vertices adjacent =
        let checkIfConvex' = checkIfConvex vertices
        
        let result =
            Map.partition checkIfConvex' adjacent
            |> (fun (c, r) -> 
            let c = Map.fold (fun state key _ -> Set.add key state) Set.empty c
            let r = Map.fold (fun state key _ -> Set.add key state) Set.empty r
            c, r)
        result

module HoleHandling =
    type OuterPolygon =
        { vertices : Map<int, Point2f>
          adjacent : Map<int, int * int>
          reflex : Set<int>
          convex : Set<int> }
    
    type HolePolygon =
        { vertices : Map<int, Point2f>
          adjacent : Map<int, int * int> }
    
    type Edge =
        { v0 : int * Point2f
          v1 : int * Point2f }
    
    type RayIntersection =
        | Point of vKey : int
        | Edge of edge : Edge * point : Point2f
    
    module PolygonSimplifier =
        let private addHoleVertices (polygon : OuterPolygon) hole =
            let vertices' =
                Map.fold (fun vertices vKey vPoint -> Map.add vKey vPoint vertices) polygon.vertices hole.vertices
            let adjacent' =
                Map.fold (fun adjacent vKey vPoint -> Map.add vKey vPoint adjacent) polygon.adjacent hole.adjacent
            { polygon with vertices = vertices'
                           adjacent = adjacent' }
        
        let private duplicateVertice (polygon : OuterPolygon) vKey =
            let vPoint = Map.find vKey polygon.vertices
            let newVKey = Map.count polygon.vertices
            let vertices = Map.add newVKey vPoint polygon.vertices
            let polygon' = { polygon with vertices = vertices }
            newVKey, polygon'
        
        let private updateAdjacent (polygon : OuterPolygon) outerVKey outerNewVKey innerVKey innerNewVKey =
            let outerPrevVKey, outerNextVKey = Map.find outerVKey polygon.adjacent
            let _, outerNextNextVKey = Map.find outerNextVKey polygon.adjacent
            let innerPrevVKey, innerNextVKey = Map.find innerVKey polygon.adjacent
            let innerPrevPrevVKey, _ = Map.find innerPrevVKey polygon.adjacent
            let outerAdjacent' = outerPrevVKey, innerVKey
            let outerNewAdjacent = innerNewVKey, outerNextVKey
            let outerNextAdjacent' = outerNewVKey, outerNextNextVKey
            let innerAdjacent' = outerVKey, innerNextVKey
            let innerNewAdjacent = innerPrevVKey, outerNewVKey
            let innerPrevAdjacent' = innerPrevPrevVKey, innerNewVKey
            
            let adjacent' =
                polygon.adjacent
                |> Map.add outerVKey outerAdjacent'
                |> Map.add outerNewVKey outerNewAdjacent
                |> Map.add outerNextVKey outerNextAdjacent'
                |> Map.add innerVKey innerAdjacent'
                |> Map.add innerNewVKey innerNewAdjacent
                |> Map.add innerPrevVKey innerPrevAdjacent'
            { polygon with adjacent = adjacent' }
        
        let private updateConvex (outer : OuterPolygon) inner outerVKey newOuterVKey newInnerVKey =
            let isConvex vKey (outer : OuterPolygon) =
                let prevVKey, nextVKey = Map.find vKey outer.adjacent
                let prevVPoint = Map.find prevVKey outer.vertices
                let vPoint = Map.find vKey outer.vertices
                let nextVPoint = Map.find nextVKey outer.vertices
                
                let convex', reflex' =
                    match PolygonEngine.vertexIsConvex prevVPoint vPoint nextVPoint with
                    | true -> 
                        let convex = Set.add vKey outer.convex
                        let reflex = Set.remove vKey outer.reflex
                        convex, reflex
                    | false -> 
                        let convex = Set.remove vKey outer.convex
                        let reflex = Set.add vKey outer.reflex
                        convex, reflex
                { outer with convex = convex'
                             reflex = reflex' }
            
            let innerVerticeKeyList =
                inner.vertices
                |> Map.toList
                |> List.map fst
            
            let outer =
                List.fold (fun outer vKey -> isConvex vKey outer) outer innerVerticeKeyList
                |> isConvex outerVKey
                |> isConvex newOuterVKey
                |> isConvex newInnerVKey
            
            outer
        
        let simplify outer hole polygonVKey holeVKey =
            let outer = addHoleVertices outer hole
            let newOuterVKey, outer = duplicateVertice outer polygonVKey
            let newHoleVKey, outer = duplicateVertice outer holeVKey
            let outer = updateAdjacent outer polygonVKey newOuterVKey holeVKey newHoleVKey
            let outer = updateConvex outer hole polygonVKey newOuterVKey newHoleVKey
            outer
    
    let private calcPolygon raw =
        let mapRawVertex state v = Map.add (Map.count state) v state
        let vertices = List.fold mapRawVertex Map.empty raw.points
        let adjacent = PolygonEngine.getAdjacent vertices
        let convex, reflex = PolygonEngine.getConvexReflexVertices vertices adjacent
        
        let polygon =
            { vertices = vertices
              adjacent = adjacent
              reflex = reflex
              convex = convex }
        polygon
    
    let private calcHole raw startKey =
        let vertices =
            raw.points
            |> List.mapi (fun index v -> (startKey + index, v))
            |> Map.ofList
        
        let adjacent = PolygonEngine.getAdjacent vertices
        { vertices = vertices
          adjacent = adjacent }
    
    let private findSuitableHoleVertice hole =
        let maxX =
            hole.vertices
            |> Map.toList
            |> List.map (fun (k, v) -> v.x)
            |> List.max
        hole.vertices
        |> Map.toList
        |> List.filter (fun (v, p) -> p.x = maxX)
        |> List.minBy (fun (v, p) -> p.y)
    
    let private findIntersectionWithDistance (ray : LineSegment) edge =
        let v0Key, p0 = edge.v0
        let v1Key, p1 = edge.v1
        if p0.x < p1.x && (collinearPoints ray.p0 p0 ray.p1) then 
            let distance = lineLength ray.p0 p0
            Some(RayIntersection.Point(v0Key), distance)
        elif (collinearPoints ray.p0 p1 ray.p1) then 
            let distance = lineLength ray.p0 p1
            Some(RayIntersection.Point(v1Key), distance)
        else 
            match lineSegmentsIntersect ray { p0 = p0
                                              p1 = p1 } with
            | Some(point) -> 
                let distance = lineLength ray.p0 point
                Some(RayIntersection.Edge(edge, point), distance)
            | None -> None
    
    let private getEdge vertices v0Key v1Key =
        let p0 = Map.find v0Key vertices
        let p1 = Map.find v1Key vertices
        { v0 = (v0Key, p0)
          v1 = (v1Key, p1) }
    
    let private edgesToTheLeft p (edge : Edge) =
        let _, p0 = edge.v0
        let _, p1 = edge.v1
        (lineSide p0 p1 p) = -1
    
    let private findRayIntersection (polygon : OuterPolygon) holePoint =
        let rayMaxX = 100000.0f
        
        let ray =
            { LineSegment.p0 = holePoint
              p1 =
                  { x = rayMaxX
                    y = holePoint.y } }
        
        let findIntersection = findIntersectionWithDistance ray
        let getEdge' = getEdge polygon.vertices
        
        let (intersection, _) =
            polygon.adjacent
            |> Map.toList
            |> List.map (fun (vKey, (_, vNextKey)) -> getEdge' vKey vNextKey)
            |> List.filter (edgesToTheLeft ray.p0) // only those edges where hole vertice is to the left
            |> List.map findIntersection
            |> List.choose (fun item -> item)
            |> List.minBy (fun (_, distance) -> distance)
        intersection
    
    let private maxXEdgeVertice edge =
        let _, p0 = edge.v0
        let _, p1 = edge.v1
        if p1.x > p0.x then edge.v1
        else edge.v0
    
    let private processEdgeIntersection polygon holePoint edge intersectionPoint =
        // H -- hole point
        // V -- outer polygon point
        // I -- intersection point
        // E -- edge point
        let vEKey, pE = maxXEdgeVertice edge
        let inTriangle = Math.pointInTriangle holePoint intersectionPoint pE
        
        let insideTriangle =
            polygon.reflex
            |> List.ofSeq
            |> List.filter (fun vKey -> vKey <> vEKey)
            |> List.filter (fun vKey -> 
                   let p = Map.find vKey polygon.vertices
                   inTriangle p)
        
        let get_H_V_relation vKey =
            let vPoint = Map.find vKey polygon.vertices
            
            let H_I =
                { LineSegment.p0 = holePoint
                  p1 = intersectionPoint }
            
            let H_V =
                { LineSegment.p0 = holePoint
                  p1 = vPoint }
            
            let distance = lineLength H_V.p0 H_V.p1
            let angle = lineSegmentsAngle H_I H_V
            vKey, angle, distance
        
        let compare_H_V_relations (v1, angle1, dist1) (v2, angle2, dist2) =
            if angle1 > angle2 then 1
            else if angle1 < angle2 then -1
            else if dist1 > dist2 then 1
            else -1
        
        match insideTriangle with
        | [] -> vEKey
        | vertices -> 
            let vKey, _, _ =
                vertices
                |> List.map get_H_V_relation
                |> List.sortWith compare_H_V_relations
                |> List.head
            vKey
    
    let private findSuitablePolygonVKey (polygon : OuterPolygon) holePoint =
        let intersection = findRayIntersection polygon holePoint
        
        let polygonVKey =
            match intersection with
            | RayIntersection.Point(vKey) -> vKey
            | RayIntersection.Edge(edge, intersectionPoint) -> 
                processEdgeIntersection polygon holePoint edge intersectionPoint
        polygonVKey
    
    /// simplifies an outer polygon by removing a hole
    let private removeHole (polygon : OuterPolygon) rawHole =
        let hole = calcHole rawHole (Map.count polygon.vertices)
        let holeVKey, holePoint = findSuitableHoleVertice hole
        let polygonVKey = findSuitablePolygonVKey polygon holePoint
        let polygon' = PolygonSimplifier.simplify polygon hole polygonVKey holeVKey
        polygon'
    
    let removeHoles outer holes =
        let polygon = calcPolygon outer
        let polygon' = List.fold removeHole polygon holes
        polygon'

module TriangulationEngine =
    type Polygon =
        { vertices : Map<int, Point2f>
          adjacent : Map<int, int * int>
          convex : Set<int>
          reflex : Set<int>
          earTips : Set<int> }
    
    let vertexIsEarTip p0 p1 p2 points =
        let pointInTriangle' = Math.pointInTriangle p0 p1 p2
        let earTip = List.exists pointInTriangle' points |> not
        earTip
    
    let removeEarTip earTipKey polygon =
        let convex = Set.remove earTipKey polygon.convex
        let earTips = Set.remove earTipKey polygon.earTips
        let adjacent = Map.remove earTipKey polygon.adjacent
        let vertices = Map.remove earTipKey polygon.vertices
        { polygon with convex = convex
                       earTips = earTips
                       adjacent = adjacent
                       vertices = vertices }
    
    let updateAdjacent (prevKey, nextKey) polygon =
        let (prevPrevKey, _) = Map.find prevKey polygon.adjacent
        let (_, nextNextKey) = Map.find nextKey polygon.adjacent
        let newPrevAdjacent = (prevPrevKey, nextKey)
        let newNextAdjacent = (prevKey, nextNextKey)
        
        let adjacent =
            polygon.adjacent
            |> Map.add prevKey newPrevAdjacent
            |> Map.add nextKey newNextAdjacent
        { polygon with adjacent = adjacent }
    
    let updateReflexConvex vKey polygon =
        let reflex, convex =
            match Set.contains vKey polygon.reflex with
            | true -> 
                let vAdjacent = Map.find vKey polygon.adjacent
                match PolygonEngine.checkIfConvex polygon.vertices vKey vAdjacent with
                | true -> 
                    let convex = Set.add vKey polygon.convex
                    let reflex = Set.remove vKey polygon.reflex
                    reflex, convex
                | false -> polygon.reflex, polygon.convex
            | false -> polygon.reflex, polygon.convex
        { polygon with convex = convex
                       reflex = reflex }
    
    let updateEarTips vKey polygon =
        let earTips =
            match Set.contains vKey polygon.convex with
            | true -> 
                match PolygonEngine.checkPoints polygon.vertices polygon.adjacent polygon.reflex vKey with
                | true -> Set.add vKey polygon.earTips
                | false -> Set.remove vKey polygon.earTips
            | false -> polygon.earTips
        { polygon with earTips = earTips }
    
    let updateEars vertices v0Key v1Key v2Key ears =
        let point0 = Map.find v0Key vertices
        let point1 = Map.find v1Key vertices
        let point2 = Map.find v2Key vertices
        { v0 = point0
          v1 = point1
          v2 = point2 }
        :: ears
    
    let rec trian polygon ears =
        let earTipKey = Set.minElement polygon.earTips
        let (prevKey, nextKey) = Map.find earTipKey polygon.adjacent
        let ears = updateEars polygon.vertices prevKey earTipKey nextKey ears
        
        let update =
            removeEarTip earTipKey
            >> updateAdjacent (prevKey, nextKey)
            >> updateReflexConvex prevKey
            >> updateReflexConvex nextKey
            >> updateEarTips prevKey
            >> updateEarTips nextKey
        
        let polygon = update polygon
        match Map.count polygon.vertices with
        | c when c = 3 -> 
            let earTipKey = Set.minElement polygon.earTips
            let (prevKey, nextKey) = Map.find earTipKey polygon.adjacent
            let p0 = Map.find prevKey polygon.vertices
            let p1 = Map.find earTipKey polygon.vertices
            let p2 = Map.find nextKey polygon.vertices
            { v0 = p0
              v1 = p1
              v2 = p2 }
            :: ears
        | _ -> trian polygon ears
    
    let calcPolygon (outer : HoleHandling.OuterPolygon) =
        let checker = PolygonEngine.checkPoints outer.vertices outer.adjacent outer.reflex
        let earTips = Set.filter checker outer.convex
        
        let polygon =
            { vertices = outer.vertices
              adjacent = outer.adjacent
              convex = outer.convex
              reflex = outer.reflex
              earTips = earTips }
        polygon
    
    let triangulate outer holes =
        let outer' = HoleHandling.removeHoles outer holes
        let polygon = calcPolygon outer'
        trian polygon []
