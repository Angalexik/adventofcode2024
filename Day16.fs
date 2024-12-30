module Day16

open AdventUtils
open System
open Utils
open Utils.Func
open Utils.Array
open QuikGraph
open QuikGraph.Algorithms
open System.Text

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

let debugPrint (grid: _ array2d) positions =
    let sb = new StringBuilder()

    for y = 0 to Array2D.length1 grid - 1 do
        for x = 0 to Array2D.length2 grid - 1 do
            if Seq.contains (y, x) positions then
                sb.Append "O" |> ignore
            else
                grid[y, x] |> _.ToString() |> sb.Append |> ignore

        sb.AppendLine() |> ignore

    printfn $"{sb.ToString()}\n\n\n"

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
    0u
    // pos -- goalPos ||> (fun y x -> (y * y + x * x) |> float |> Math.Sqrt |> Math.Floor |> uint32)

let weight start finish =
    if fst start <> fst finish then 1000u else 1u
    // elif (snd finish, snd start) ||> heuristic = 1u then 1u
    // else failwith "You probably messed this one up"

let astar (grid: _ array2d) startPos endPos =
    let h = heuristic endPos
    let possible = possibleMoves grid
    let start = (East, startPos)
    let openSet = BinomialHeapPQ.empty |> BinomialHeapPQ.insert (h startPos) start
    let gScores = Map.ofList [ (start, 0u) ]

    let unravel parents =
        let rec loop visited queue =
            match queue with
            | [] -> visited
            | (x :: xs) ->
                if List.contains x visited then
                    loop visited xs
                else
                    let children = Map.tryFind x parents |> Option.defaultValue []
                    // Seq.length children |> dbg |> ignore
                    loop (x :: visited) (children @ xs)

        loop [] ([ East; West; North; South ] |> List.map (fun d -> (d, endPos)))
        |> Seq.map snd
        |> Set.ofSeq

    let rec loop openSet visited gScores shortestLength parents =
        let getGScore n =
            Map.tryFind n gScores |> Option.defaultValue UInt32.MaxValue

        let current = BinomialHeapPQ.getMin openSet |> Option.get // RIPBOZO
        let curSpot = current.v
        let atEnd = snd curSpot = endPos

        match shortestLength with
        // shortestLength exists and current path to end is longer
        | Some c when current.k > c && atEnd -> (unravel parents, c)
        | _ ->
            let openSet = BinomialHeapPQ.deleteMin openSet
            let visited = if not atEnd then Set.add curSpot visited else visited
            let neighbours = possible curSpot

            let neigbourCosts =
                neighbours
                |> Seq.filter ((flip Set.contains visited) >> not)
                |> Seq.map (fun n -> n, getGScore curSpot + weight curSpot n)
                |> Seq.filter (fun (n, c) -> c <= getGScore n)

            let (newGScores, newOpenSet, newParents) =
                neigbourCosts
                |> Seq.fold
                    (fun (gScores, openSet, parents) (n, c) ->
                        let score = c + h (snd n)
                        let newOpenSet = BinomialHeapPQ.insert score n openSet
                        let oldGScore = getGScore n

                        let newParents =
                            if oldGScore = c then
                                Map.change
                                    n
                                    (Option.map (fun ps -> curSpot :: ps))
                                    parents
                            else
                                Map.add n [ curSpot ] parents

                        let newGScores = Map.add n c gScores
                        (newGScores, newOpenSet, newParents))
                    (gScores, openSet, parents)

            let newShortestLength =
                if atEnd then
                    shortestLength |> Option.orElse (Some current.k)
                else
                    shortestLength

            loop newOpenSet visited newGScores newShortestLength newParents

    loop openSet Set.empty gScores None Map.empty

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

    let (positions, cost) = astar input startPos endPos
    debugPrint input Set.empty
    debugPrint input positions
    positions |> Set.count

let test () =
    let solution = (dayTestInputs 16).[1] |> parse1 |> solve2
    printfn $"{solution}"

let part1 () =
    printfn $"Part 1: {dayInput 16 |> parse1 |> solve1}"

let part2 () =
    printfn $"Part 2: {dayInput 16 |> parse1 |> solve2}"
