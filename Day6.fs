module Day6

open AdventUtils
open Utils.Text
open System
open System.Collections.Generic
open System.Diagnostics
open ShellProgressBar

// Part1: 1:05:35
// Total paused at: 3:50:42

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

let solve1 (grid, guardPos: int * int) =
    let isOOB pos =
        List.contains (fst pos) [ 0 .. Array2D.length1 grid - 1 ] |> not
        || List.contains (snd pos) [ 0 .. Array2D.length2 grid - 1 ] |> not

    let isBlocked pos = grid.[fst pos, snd pos]

    let rec loop visited pos direction =
        if travel direction pos |> isOOB then
            Set.count visited + 1
        elif travel direction pos |> isBlocked then
            loop visited pos (turnRight direction)
        else
            loop (Set.add pos visited) (travel direction pos) direction

    loop Set.empty guardPos Up


let isBlocked (grid: 'a array2d) pos = grid.[fst pos, snd pos]

let isCycle guardPos grid =
    let isOOB pos =
        List.contains (fst pos) [ 0 .. Array2D.length1 grid - 1 ] |> not
        || List.contains (snd pos) [ 0 .. Array2D.length2 grid - 1 ] |> not

    let isBlocked = isBlocked grid

    let rec loop (visited: (Direction * (int * int)) HashSet) pos direction =
        if travel direction pos |> isOOB then
            false
        elif travel direction pos |> isBlocked then
            loop visited pos (turnRight direction)
        else
            let beenThereDoneThat = visited.Contains((direction, pos))
            visited.Add((direction, pos)) |> ignore
            if beenThereDoneThat then
                true
            else
                loop visited (travel direction pos) direction

    loop (new HashSet<Direction * (int * int)>()) guardPos Up

// I might be cooking with this one
// Every visited position has a step number
// The step number is the first time I visited this location
// If I get to a position I've already been to, I remember its step number
// If the next position I get to is also already visited, I compare the step number of this position with
// the step number of the previous position. If they're one apart, we're in a loop (or maybe if p1 > p2)
// ---
// Let the records show that I was not, in fact, cooking
let solve2 (grid, guardPos) =
    let toInt =
        function
        | true -> 1
        | false -> 0

    let mutable pp = []

    Array2D.iteri
        (fun y x e ->
            if not e && (y, x) <> guardPos then
                pp <- (y, x) :: pp)
        grid

    let options = new ProgressBarOptions()
    options.ShowEstimatedDuration <- true
    use pbar = new ProgressBar(pp.Length, "Progress...")

    pp
    |> List.toArray
    |> Array.mapi (fun i (y, x) ->
        let newGrid = Array2D.copy grid
        newGrid.[y, x] <- true
        i, newGrid)
    |> Array.Parallel.sumBy (fun (i, newGrid) ->
        let stopwatch = Stopwatch.StartNew()
        let ret = newGrid |> isCycle guardPos |> toInt
        stopwatch.Stop()
        pbar.Tick($"Number {i} of {pp.Length}: ETA: {(pp.Length |> float) * stopwatch.Elapsed.TotalMinutes}")
        ret)
// |> List.sumBy (isCycle guardPos >> toInt)

let test () =
    let solution = (dayTestInputs 6).[0] |> parse1 |> solve2
    // let solution = dayTestInputs 6 |> Array.map (parse1 >> solve2)
    printfn "%A" solution

let part1 () =
    printfn $"Part 1: {dayInput 6 |> parse1 |> solve1}"

let part2 () =
    printfn $"Part 2: {dayInput 6 |> parse1 |> solve2}"
