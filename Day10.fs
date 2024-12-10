module Day10

open AdventUtils
open System
open Utils.Text
open Utils.Func
open Utils.Array

let (++) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

let charToInt =
    function
    | '.' -> -100
    | c -> int c - int '0'

let parse1 input =
    input |> charGrid |> Array2D.mapi (fun y x e -> ((y, x), charToInt e))

let solve1 input =
    let neighbours graph (element: (int * int) * int) =
        let offsets = [ (1, 0); (-1, 0); (0, 1); (0, -1) ]
        let tryGet (y, x) a = (Array2D.get a y |> tryF) x

        offsets
        |> List.choose (fun offset ->
            let idx = offset ++ (fst element)
            tryGet idx graph)
        |> List.filter (fun (_, height) -> height = snd element + 1)

    let rec dfs graph visited toVisit =
        match toVisit with
        | [] -> visited |> List.filter (snd >> (=) 9) |> List.length
        | (x :: xs) ->
            if List.contains x visited then
                dfs graph visited xs
            else
                let neighbouring = neighbours graph x
                dfs graph (x :: visited) (neighbouring @ xs)

    input
    |> Array2D.map (fun e -> if snd e = 0 then Some <| dfs input [] [ e ] else None)
    |> flat2Darray
    |> Seq.sumBy (function
        | None -> 0
        | Some(x) -> x)

// Part2 time so far: 2:35:34
let solve2 input =
    let neighbours graph (element: (int * int) * int) =
        let offsets = [ (1, 0); (-1, 0); (0, 1); (0, -1) ]
        let tryGet (y, x) a = (Array2D.get a y |> tryF) x

        offsets
        |> List.choose (fun offset ->
            let idx = offset ++ (fst element)
            tryGet idx graph)
        |> List.filter (fun (_, height) -> height = snd element + 1)

    let rec dfs graph visited toVisit =
        match toVisit with
        | [] -> visited |> List.filter (snd >> (=) 9)
        | (x :: xs) ->
            if List.contains x visited then
                dfs graph visited xs
            else
                let neighbouring = neighbours graph x
                dfs graph (x :: visited) (neighbouring @ xs)

    let rec bfs graph visited toVisit =
        match toVisit with
        | [] -> visited
        | (x :: xs) ->
            if List.contains x visited then
                bfs graph visited xs
            else
                let neighbouring = neighbours graph x
                bfs graph (x :: visited) (xs @ neighbouring)

    let paths graph start dest =
        let sorted = bfs graph [] [ start ] |> dbg
        let mutable results = Map.ofList [ (dest, 1) ]

        sorted
        |> List.iter (fun vert ->
            neighbours graph vert
            |> List.iter (fun neighbour ->
                results <-
                    Map.change
                        vert
                        (function
                        | None -> Some(results.[neighbour])
                        | Some(x) -> Some(x + results.[neighbour]))
                        results))

        results.[start]

    input
    |> Array2D.map (fun e -> if snd e = 0 then (e, dfs input [] [ e ]) |> Some else None)
    |> flat2Darray
    |> Seq.sumBy (function
        | None -> 0
        | Some((start, nines)) -> nines |> List.sumBy (fun dest -> paths input start dest))

let test () =
    let solution = (dayTestInputs 10).[0] |> parse1 |> solve1
    printfn $"{solution}"

let part1 () =
    printfn $"Part 1: {dayInput 10 |> parse1 |> solve1}"

let part2 () =
    printfn $"Part 2: {dayInput 10 |> parse1 |> solve2}"
