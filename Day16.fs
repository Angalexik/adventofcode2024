module Day16

open AdventUtils
open System
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

let shortestPath (graph: IVertexAndEdgeListGraph<_, _>) startPos endPos =
    let weights (edge: IEdge<_>) =
        if fst edge.Source <> fst edge.Target then 1000.0 else 1.0

    let cost vert =
        snd vert -- endPos ||> (fun y x -> y * y + x * x |> float |> Math.Sqrt)

    let shortest = graph.ShortestPathsAStar(weights, cost, (East, startPos)) |> convert
    // let shortest: (Dir * (int * int)) -> seq<_> option = graph.ShortestPathsAStar (weights, cost, (East, startPos)) |> convert

    let pathLength path = path |> Seq.sumBy weights

    [ North; East; South; West ]
    |> Seq.choose ((fun dir -> (dir, endPos)) >> shortest >> Option.map pathLength)
    |> Seq.min

// This ain't it chief
let dijkstra (grid: GridElement array2d) startPos =
    let mutable distances = Map.ofList [ ((East, startPos), 0) ]

    grid
    |> Array2D.iteri (fun y x e ->
        match e with
        | Space Start
        | Wall -> ()
        | _ ->
            [ South; North; West; East ]
            |> List.iter (fun d ->
                distances <- Map.add (d, (y, x)) Int32.MaxValue distances))

    let rec loop queue (dists: Map<_, _>) =
        match queue with
        | [] -> dists
        | _ ->
            let (dir, pos) = queue |> Seq.minBy (fun u -> dists.[u])
            let queue = List.filter (fun x -> x <> (dir, pos)) queue

            let neighbouring =
                neighbours grid pos dir
                |> Seq.filter (fun (_, d, p) -> List.contains (d, p) queue)

            let dists =
                neighbouring
                |> Seq.fold
                    (fun (acc: Map<_, _>) (cost, d, p) ->
                        let alt = acc.[(dir, pos)] + cost
                        if alt < acc.[(d, p)] then Map.add (d, p) alt acc else acc)
                    dists

            loop queue dists

    distances
    |> loop (Map.keys distances |> Seq.toList)
    |> Map.pick (fun (_, (posX, posY)) d ->
        if grid.[posX, posY] = Space End then Some d else None)

let parse1 input =
    charGrid input |> Array2D.map gridElement

let solve1 input =
    let prepared =
        input
        |> Array2D.mapi (fun y x e -> ((y, x), e))
        |> flat2Darray
    let startPos =
        prepared
        |> Seq.find (snd >> (=) (Space Start))
        |> fst
    let endPos =
        prepared
        |> Seq.find (snd >> (=) (Space End))
        |> fst

    shortestPath (createGraph input) startPos endPos
    // dijkstra input startPos

let solve2 input = ()

let test () =
    let solution = (dayTestInputs 16).[0] |> parse1 |> solve1
    printfn $"{solution}"

let part1 () =
    printfn $"Part 1: {dayInput 16 |> parse1 |> solve1}"

let part2 () =
    printfn $"Part 2: {dayInput 16 |> solve2}"
