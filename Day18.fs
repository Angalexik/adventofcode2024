module Day18

open AdventUtils
open System
open Utils
open Utils.Func

let (++) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

let neighbours grid (element: int * int) =
    let offsets = [ (1, 0); (-1, 0); (0, 1); (0, -1) ]
    let tryGet (y, x) a = (Array2D.get a y |> tryF) x

    offsets
    |> List.choose (fun offset ->
        let idx = offset ++ element
        let valid = tryGet idx grid |> Option.defaultValue false
        if valid then Some idx else None)

let shortestPath grid =
    let goal = (Array2D.length1 grid) - 1

    let rec reconstruct parents path next =
        match Map.tryFind next parents with
        | None -> None
        | Some(0, 0) -> Some path
        | Some nope -> reconstruct parents (nope :: path) nope

    let rec bfs visited toVisit parents =
        match toVisit with
        | [] -> reconstruct parents [ (goal, goal) ] (goal, goal)
        | (x :: xs) ->
            if List.contains x visited then
                bfs visited xs parents
            else
                let neighbouring = neighbours grid x

                bfs
                    (x :: visited)
                    (xs @ neighbouring)
                    (Seq.fold
                        (fun map neigh ->
                            if Map.containsKey neigh map then
                                map
                            else
                                Map.add neigh x map)
                        parents
                        neighbouring)

    bfs [] [ (0, 0) ] Map.empty

let parse1 input =
    input
    |> Text.lines
    |> Seq.map (Text.splitOnce "," >> (fun (x, y) -> (int x, int y)))
    |> Seq.toList

let solve1 size dropped input =
    let mutable grid = Array2D.create size size true
    input |> Seq.take dropped |> Seq.iter (fun (x, y) -> grid[y, x] <- false)

    shortestPath grid |> Option.get |> Seq.length

let solve2 size input =
    let mutable grid = Array2D.create size size true

    let rec loop curPath ((x, y) :: bytes) =
        grid[y, x] <- false

        if List.contains (y, x) curPath then
            let newPath = shortestPath grid

            match newPath with
            | None -> (x, y)
            | Some p -> loop p bytes
        else
            loop curPath bytes

    loop (shortestPath grid |> Option.get) input

let test () =
    let solution = (dayTestInputs 18).[0] |> parse1 |> solve2 7
    printfn $"{solution}"

let part1 () =
    printfn $"Part 1: {dayInput 18 |> parse1 |> solve1 71 1024}"

let part2 () =
    printfn $"Part 2: {dayInput 18 |> parse1 |> solve2 71}"
