module Day11

open AdventUtils
open System
open Utils.Text
open Utils.Func

let applyRules stones =
    let splitNum number =
        let str = number.ToString()
        let length = str.Length / 2
        let firstHalf = str.Substring(0, length) |> int64
        let secondHalf = str.Substring(length) |> int64
        [ firstHalf; secondHalf ]

    List.collect
        (fun stone ->
            match stone with
            | 0L -> [ 1L ]
            | even when (stone.ToString().Length % 2 = 0) -> splitNum even
            | n -> [ n * 2024L ])
        stones

let parse1 input =
    split " " input |> Array.map int64 |> Array.toList

let solve1 input =
    (repeat 25 applyRules) input |> List.length

let solve2 input =
    let mutable start = input

    [ 1..75 ]
    |> Seq.iter (fun i ->
        printfn $"{i}"
        start <- applyRules start)

    List.length start

let test () =
    let inputs = (dayTestInputs 11)
    let test1 = inputs.[0] |> parse1 |> applyRules
    dbg test1 |> ignore
    let test2 = inputs.[1] |> parse1 |> (repeat 6 applyRules)
    dbg test2 |> ignore
// let solution = (dayTestInputs 11).[0] |> solve1
// printfn $"{solution}"

let part1 () =
    printfn $"Part 1: {dayInput 11 |> parse1 |> solve1}"

let part2 () =
    printfn $"Part 2: {dayInput 11 |> parse1 |> solve2}"
