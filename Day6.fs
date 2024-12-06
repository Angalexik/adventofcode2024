module Day6
#nowarn "57"

open AdventUtils
open Utils.Text
open System
open System.Collections.Generic

type Direction =
    | Up
    | Down
    | Left
    | Right

let turnRight =
    function
    | Up -> Right
    | Down -> Left
    | Left -> Up
    | Right -> Down

let travel direction (y, x) =
    match direction with
    | Up -> (y - 1, x)
    | Down -> (y + 1, x)
    | Left -> (y, x - 1)
    | Right -> (y, x + 1)

let parse1 input =
    let rows = split "\n" input |> Array.map _.ToCharArray()
    let map = Array2D.init rows.Length rows.[0].Length (fun y x -> rows.[y].[x])
    let mutable guardPos = None

    Array2D.iteri
        (fun y x e ->
            if e = '^' then
                guardPos <- Some((y, x)))
        map

    map |> Array2D.map (fun e -> e = '#'), guardPos.Value

let isOOB grid pos =
    List.contains (fst pos) [ 0 .. Array2D.length1 grid - 1 ] |> not
    || List.contains (snd pos) [ 0 .. Array2D.length2 grid - 1 ] |> not

let isBlocked (grid: 'a array2d) pos = grid.[fst pos, snd pos]

let toString grid =
    Array2D.map (fun b -> if b then '#' else '.') grid

let solve1 (grid, guardPos: int * int) =
    let rec loop visited pos direction =
        if travel direction pos |> isOOB grid then
            Set.count visited + 1
        elif travel direction pos |> isBlocked grid then
            loop visited pos (turnRight direction)
        else
            loop (Set.add pos visited) (travel direction pos) direction

    loop Set.empty guardPos Up

let isCycle guardPos grid =
    let rec loop (visited: (Direction * (int * int)) HashSet) pos direction =
        if travel direction pos |> isOOB grid then
            false
        elif travel direction pos |> isBlocked grid then
            loop visited pos (turnRight direction)
        elif visited.Contains((direction, pos)) then
            true
        else
            visited.Add((direction, pos)) |> ignore
            loop visited (travel direction pos) direction

    loop (new HashSet<Direction * (int * int)>()) guardPos Up

// Slow, but it's late and I'm tired
let solve2 (grid, guardPos) =
    let toInt =
        function
        | true -> 1
        | false -> 0

    let rec positions visited pos direction =
        if travel direction pos |> isOOB grid then
            Set.add pos visited
        elif travel direction pos |> isBlocked grid then
            positions visited pos (turnRight direction)
        else
            positions (Set.add pos visited) (travel direction pos) direction

    positions Set.empty guardPos Up
    |> Set.toArray
    |> Array.map (fun (y, x) ->
        let newGrid = Array2D.copy grid
        newGrid.[y, x] <- true
        newGrid)
    |> Array.Parallel.sumBy (isCycle guardPos >> toInt)

let test () =
    let solution = (dayTestInputs 6).[0] |> parse1 |> solve2
    // let solution = dayTestInputs 6 |> Array.map (parse1 >> solve2)
    printfn "%A" solution

let part1 () =
    printfn $"Part 1: {dayInput 6 |> parse1 |> solve1}"

let part2 () =
    printfn $"Part 2: {dayInput 6 |> parse1 |> solve2}"
