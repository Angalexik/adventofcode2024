module Day2

open AdventUtils
open System

let processReport (report: string array) =
    let isValid' (valid, decreasing) (x, y) =
        let diff = x - y
        let withinRange = diff <> 0 && (Math.Abs diff <= 3)
        let currentlyDecreasing = diff > 0

        match (valid, decreasing) with
        | false, _ -> (false, None)
        | true, Some(dec) -> (withinRange && currentlyDecreasing = dec, decreasing)
        | true, None -> (withinRange, Some(currentlyDecreasing))

    report
    |> Array.map int
    |> Array.pairwise
    |> Array.fold isValid' (true, None)
    |> fst

let removeElement at array =
    array
    |> Array.mapi (fun i e -> (i <> at, e))
    |> Array.choose (fun (keep, e) -> if keep then Some e else None)

let parse =
    (fun (t: string) -> t.Split('\n'))
    >> Array.map (fun (l: string) -> l.Split(" "))

let solve1 input =
    input |> Array.filter processReport |> _.Length

let solve2 input =
    let isSafeWithRemoved report =
        let count = Array.length report

        report
        |> Array.replicate count
        |> Array.mapi removeElement
        |> Array.exists processReport

    let either f g x = f x || g x
    input |> Array.filter (either processReport isSafeWithRemoved) |> _.Length

let test () =
    let solution = (dayTestInputs 2).[0] |> parse |> solve2
    printfn $"{solution}"

let part1 () =
    printfn $"Part 1: {dayInput 2 |> parse |> solve1}"

let part2 () =
    printfn $"Part 2: {dayInput 2 |> parse |> solve2}"
