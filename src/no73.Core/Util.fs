namespace no73.Core

module Heap =
    type HeapTree<'value> =
        | Empty
        | Node of value : 'value * rank : int * leftChild : HeapTree<'value> * rightChild : HeapTree<'value>
    
    type Heap<'value> =
        { comparer : 'value -> 'value -> int
          tree : HeapTree<'value> }
    
    let private createNode value = Node(value, 1, Empty, Empty)
    
    let private rank tree =
        match tree with
        | Empty -> 0
        | Node(_, rnk, _, _) -> rnk
    
    let rec private merge comparer tree1 tree2 =
        match tree1, tree2 with
        | Empty, tree | tree, Empty -> tree
        | Node(value1, _, lc1, rc1), Node(value2, _, _, _) -> 
            if comparer value1 value2 = 1 then merge comparer tree2 tree1
            else 
                let merged = merge comparer rc1 tree2
                let rankLeft = rank lc1
                let rankRight = rank merged
                if rankLeft >= rankRight then Node(value1, rankRight + 1, lc1, merged)
                else Node(value1, rankLeft + 1, merged, lc1)
    
    let newHeap comparer =
        { comparer = comparer
          tree = Empty }
    
    let insert value heap =
        let tree' = merge heap.comparer (createNode value) heap.tree
        { heap with tree = tree' }
    
    let getRoot heap =
        match heap.tree with
        | Empty -> None
        | Node(value, _, _, _) -> Some(value)
    
    let deleteRoot heap =
        match heap.tree with
        | Empty -> heap
        | Node(_, _, lc, rc) -> 
            let tree' = merge heap.comparer lc rc
            { heap with tree = tree' }

module AStar =
    // F(node) = G(node) + H(node)
    // where G - the cost of the path from the start to the node
    //       H - a heuristic function that estimates the cost of the cheapest path from the node to the goal
    module private Internal =
        type PathPart<'id when 'id : comparison> =
            { id : 'id
              g : float32
              f : float32
              path : 'id list }
        
        type Ctx<'id when 'id : comparison> =
            { goal : 'id
              openList : Heap.Heap<PathPart<'id>>
              closedList : Set<'id>
              adjacentGetter : 'id -> 'id Set
              heuristic : 'id -> float32
              distance : 'id -> 'id -> float32 }
        
        let calcF g h = g + h
        let comparer pathPart1 pathPart2 = compare pathPart1.f pathPart2.f
        
        let rec find ctx =
            match Heap.getRoot ctx.openList with
            | Some(current) -> 
                let openList = Heap.deleteRoot ctx.openList
                let closedList = Set.add current.id ctx.closedList
                if current.id = ctx.goal then 
                    // path found
                    let path = current.id :: current.path |> List.rev
                    Some(path)
                else 
                    let openList =
                        ctx.adjacentGetter current.id
                        |> Set.toList
                        |> List.filter (fun id -> Set.contains id closedList |> not)
                        // note: maybe additional check whether it was somewhere in the path of the current?
                        |> List.map (fun id -> 
                               let path = current.id :: current.path
                               let distance = ctx.distance current.id id
                               let g = current.g + distance
                               let h = ctx.heuristic id
                               let f = calcF g h
                               { id = id
                                 g = g
                                 f = f
                                 path = path })
                        |> List.fold (fun ol neighbor -> Heap.insert neighbor ol) openList
                    find { ctx with openList = openList
                                    closedList = closedList }
            | None -> None
    
    open Internal
    
    type Cmd<'id when 'id : comparison> =
        { start : 'id
          goal : 'id
          adjacentGetter : 'id -> 'id Set
          heuristic : 'id -> float32
          distance : 'id -> 'id -> float32 }
    
    let find (cmd : Cmd<'id>) =
        let g = 0.0f
        let h = cmd.heuristic cmd.start
        let f = calcF g h
        
        let startPathPart =
            { id = cmd.start
              g = g
              f = f
              path = List.empty }
        
        let openList = Heap.newHeap comparer |> Heap.insert startPathPart
        
        let ctx =
            { goal = cmd.goal
              openList = openList
              closedList = Set.empty
              adjacentGetter = cmd.adjacentGetter
              heuristic = cmd.heuristic
              distance = cmd.distance }
        Internal.find ctx