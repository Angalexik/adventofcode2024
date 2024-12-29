module Day16

open AdventUtils
open System
open Utils
open Utils.Func
open Utils.Array
open QuikGraph
open QuikGraph.Algorithms

type Dir =
    | East
    | West
    | North
    | South

let rotateClockwise =
    function
    | East -> South
    | South -> West
    | West -> North
    | North -> East

let rotateCounterClockwise =
    function
    | East -> North
    | North -> West
    | West -> South
    | South -> East


type SpaceThing =
    | Start
    | End
    | Nothing

type GridElement =
    | Wall
    | Space of SpaceThing

    override this.ToString() : string =
        match this with
        | Wall -> "#"
        | Space Nothing -> "."
        | Space Start -> "S"
        | Space End -> "E"

let gridElement =
    function
    | '#' -> Wall
    | '.' -> Space Nothing
    | 'S' -> Space Start
    | 'E' -> Space End
    | _ -> failwith "Naaaah"

let (++) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
let (--) (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

let moveTo start dir =
    let add = (++) start

    match dir with
    | North -> add (-1, 0)
    | South -> add (1, 0)
    | East -> add (0, 1)
    | West -> add (0, -1)

let neighbours (graph: GridElement array2d) pos currentDir =
    let offsets =
        Map.ofList [ (South, (1, 0)); (North, (-1, 0)); (West, (0, 1)); (East, (0, -1)) ]

    offsets
    |> Map.map (fun dir offset ->
        (if dir = currentDir then 1 else 1000), dir, pos ++ offset)
    |> Map.filter (fun _ (_, _, pos) -> graph[fst pos, snd pos] <> Wall)
    |> Map.values

// let neighbours graph (element: (int * int) * int) =
//     let offsets = [ (1, 0); (-1, 0); (0, 1); (0, -1) ]
//     let tryGet (y, x) a = (Array2D.get a y |> tryF) x

//     offsets
//     |> List.choose (fun offset ->
//         let idx = offset ++ (fst element)
//         tryGet idx graph)
//     |> List.filter (fun (_, height) -> height = snd element + 1)


// let shortestPath (grid: GridElement array2d) startPos endPos =
//     let distToEnd pos =
//         pos -- endPos ||> (fun y x -> Math.Sqrt (y*y + x*x))
//     ()

let possibleMoves (grid: _ array2d) (spot: Dir * (int * int)) =
    let dir, pos = spot
    let newPos = moveTo pos dir

    let forward =
        if grid[fst pos, snd pos] <> Wall then
            [ (dir, newPos) ]
        else
            []

    let rotations = [ (rotateClockwise dir, pos); (rotateCounterClockwise dir, pos) ]
    forward @ rotations

let heuristic goalPos pos =
    let abs =
        function
        | n when n < 0 -> -n
        | n -> n

    pos -- goalPos ||> (fun y x -> abs y + abs x)

let weight start finish =
    if fst start <> fst finish then 1000u
    elif (snd finish, snd start) ||> heuristic = 1 then 1u
    else failwith "You probably messed this one up"

let astar (grid: _ array2d) startPos endPos =
    let h = heuristic endPos >> uint32
    let possible = possibleMoves grid
    let start = (East, startPos)
    let openSet = BinomialHeapPQ.empty |> BinomialHeapPQ.insert (h startPos) start
    let gScores = Map.ofList [ (start, 0u) ]

    let rec loop openSet visited gScores shortestLength =
        let current = BinomialHeapPQ.getMin openSet |> Option.get // RIPBOZO
        let curSpot = current.v
        let atEnd = snd curSpot = endPos

        match shortestLength with
        // shortestLength exists and current path to end is longer
        // | Some c when current.k > c && atEnd -> (visited, current.k)
        | Some c -> (visited, c)
        | _ ->
            let openSet = BinomialHeapPQ.deleteMin openSet
            // let visited = if not atEnd then Set.add curSpot visited else visited
            let visited = Set.add curSpot visited
            let neighbours = possible curSpot

            let neigbourCosts =
                neighbours
                |> Seq.filter ((flip Set.contains visited) >> not)
                // |> Seq.filter (fun n -> 
                //     if snd n = endPos then 
                //         match shortestLength with
                //         | None -> true
                //         | Some s -> 
                //     else true)
                |> Seq.map (fun n -> n, Map.find curSpot gScores + weight curSpot n)
                |> Seq.filter (fun (n, c) ->
                    Map.tryFind n gScores |> Option.defaultValue UInt32.MaxValue |> (<) c)

            let (newGScores, newOpenSet) =
                neigbourCosts
                |> Seq.fold
                    (fun (gScores, openSet) (n, c) ->
                        let newGScores = Map.add n c gScores
                        let score = c + h (snd n)
                        let newOpenSet = BinomialHeapPQ.insert score n openSet
                        (newGScores, newOpenSet))
                    (gScores, openSet)
            let newShortestLength = if atEnd then shortestLength |> Option.orElse (Some current.k) else shortestLength

            loop newOpenSet visited newGScores newShortestLength

    loop openSet Set.empty gScores None

let createGraph (grid: GridElement array2d) =
    let mutable edges = []

    grid
    |> Array2D.iteri (fun y x e ->
        if e <> Wall then
            let pos = (y, x)

            let newEdges =
                [ North; East; South; West ]
                |> List.collect (fun dir ->
                    let newPos = moveTo pos dir

                    let straight =
                        if grid[fst newPos, snd newPos] <> Wall then
                            [ new SEdge<_>((dir, pos), (dir, newPos)) ]
                        else
                            []

                    straight
                    @ [
                        new SEdge<_>((dir, pos), (rotateClockwise dir, pos))
                        new SEdge<_>((dir, pos), (rotateCounterClockwise dir, pos))
                    ])

            edges <- edges @ newEdges)

    new ArrayAdjacencyGraph<_, _>(edges.ToAdjacencyGraph())

let convert (tryFunc: TryFunc<_, _>) arg =
    let mutable out = null
    let success = tryFunc.Invoke(arg, &out)
    if success then Some out else None

let weights (edge: IEdge<_>) =
    if fst edge.Source <> fst edge.Target then 1000.0 else 1.0

let shortestPaths (graph: IVertexAndEdgeListGraph<_, _>) startPos endPos =
    let cost vert =
        snd vert -- endPos ||> (fun y x -> y * y + x * x |> float |> Math.Sqrt)

    let shortest = graph.ShortestPathsAStar(weights, cost, (East, startPos)) |> convert

    [ North; East; South; West ]
    |> Seq.choose ((fun dir -> (dir, endPos)) >> shortest)

let parse1 input =
    charGrid input |> Array2D.map gridElement

let solve1 input =
    let prepared = input |> Array2D.mapi (fun y x e -> ((y, x), e)) |> flat2Darray
    let startPos = prepared |> Seq.find (snd >> (=) (Space Start)) |> fst
    let endPos = prepared |> Seq.find (snd >> (=) (Space End)) |> fst

    astar input startPos endPos |> snd

let solve2 input =
    let prepared = input |> Array2D.mapi (fun y x e -> ((y, x), e)) |> flat2Darray
    let startPos = prepared |> Seq.find (snd >> (=) (Space Start)) |> fst
    let endPos = prepared |> Seq.find (snd >> (=) (Space End)) |> fst

    let spots =
        shortestPaths (createGraph input) startPos endPos
        |> Seq.concat
        |> Seq.collect (fun e -> [ snd e.Source; snd e.Target ])
        |> Set.ofSeq

    ()

let test () =
    let solution = (dayTestInputs 16).[0] |> parse1 |> solve1
    printfn $"{solution}"

let part1 () =
    printfn $"Part 1: {dayInput 16 |> parse1 |> solve1}"

let part2 () =
    printfn $"Part 2: {dayInput 16 |> parse1 |> solve2}"
