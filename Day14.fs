module Day14

open AdventUtils
open System
open Utils
open System.Text.RegularExpressions
open System.Text

type Robot =
    {
        Position: int * int
        Velocity: int * int
    }


let showRobots' width height robots =
    let a2d = Array2D.create height width '.'
    let builder = new StringBuilder()
    robots |> Seq.iter (fun { Position = (px, py) } -> Array2D.set a2d py px '#')

    for y = 0 to height - 1 do
        for x = 0 to width - 1 do
            builder.Append a2d[y, x] |> ignore

        builder.AppendLine() |> ignore

    printfn $"\n\n\n{builder.ToString()}"
    robots



let showRobots width height robots =
    printfn "---------------"

    for y = 0 to height - 1 do
        for x = 0 to width - 1 do
            let count =
                robots
                |> Seq.filter (fun { Position = (px, py) } -> px = x && py = y)
                |> Seq.length

            if count = 0 then printf "." else printf $"{count}"

        printfn ""

    robots


let quadrant width height { Position = pos } =
    let topHalfCutoff = height / 2 - 1
    let botHalfCutoff = height - (topHalfCutoff + 1)
    let leftHalfCutoff = width / 2 - 1
    let rightHalfCutoff = width - (leftHalfCutoff + 1)

    match pos with
    | (px, py) when px <= leftHalfCutoff && py <= topHalfCutoff -> 0
    | (px, py) when px <= leftHalfCutoff && py >= botHalfCutoff -> 1
    | (px, py) when px >= rightHalfCutoff && py <= topHalfCutoff -> 2
    | (px, py) when px >= rightHalfCutoff && py >= botHalfCutoff -> 3
    | _ -> -1

let modLoop value divisor =
    let n = value % divisor
    if n < 0 then divisor + n else n

let step width height robots =
    let move
        {
            Position = (px, py)
            Velocity = (vx, vy)
        }
        =
        let px = px + vx
        let py = py + vy
        (modLoop px width, modLoop py height)

    Seq.map (fun robot -> { robot with Position = (move robot) }) robots

let parse1 input =
    let regex = Regex @"=(-?\d+),(-?\d+) .+=(-?\d+),(-?\d+)"

    input
    |> Text.split "\n"
    |> Array.map (
        regex.Match
        >> _.Groups
        >> Seq.tail
        >> Seq.map (_.Value >> int)
        >> Seq.toList
        >> (fun [ px; py; vx; vy ] ->
            {
                Position = (px, py)
                Velocity = (vx, vy)
            })
    )

let solve1 width height input =
    showRobots width height input |> ignore
    let robots = (Func.repeat 100 <| step width height) input
    let quadrant = quadrant width height
    let q0 = robots |> Seq.filter (quadrant >> (=) 0) |> Seq.length |> dbg
    let q1 = robots |> Seq.filter (quadrant >> (=) 1) |> Seq.length |> dbg
    let q2 = robots |> Seq.filter (quadrant >> (=) 2) |> Seq.length |> dbg
    let q3 = robots |> Seq.filter (quadrant >> (=) 3) |> Seq.length |> dbg
    q0 * q1 * q2 * q3

let solve2 width height input =
    let maxSteps = 10000
    let step' = step width height >> showRobots' width height
    (Func.repeat maxSteps step') input


let test () =
    let solution = (dayTestInputs 14).[0] |> parse1 |> solve1 11 7
    printfn "%A" solution

let part1 () =
    printfn $"Part 1: {dayInput 14 |> parse1 |> solve1 101 103}"

let part2 () =
    printfn $"Part 2: {dayInput 14 |> parse1 |> solve2 101 103}"
