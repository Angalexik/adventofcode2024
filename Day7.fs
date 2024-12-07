module Day7

open AdventUtils
open System
open Utils.Text

let parse1 input =
    let parseLine line =
        let (result, eq) = splitOnce ": " line
        let eq = eq |> split " " |> Array.toList |> List.map int64
        result |> int64, eq

    input |> split "\n" |> Array.toList |> List.map parseLine

let isValid concat (result, eq) =
    let rec loop target numbers currentRes =
        match numbers with
        | [] -> currentRes = target
        | (x :: xs) ->
            let added = currentRes + x
            let multiplied = currentRes * x
            let concated = currentRes.ToString() + x.ToString() |> int64
            let f = loop target xs

            if concat then
                f added || f multiplied || f concated
            else
                f added || f multiplied

    loop result (List.tail eq) (List.head eq)

let solve1 input =
    input |> List.filter (isValid false) |> List.sumBy fst

let solve2 input =
    input |> List.filter (isValid true) |> List.sumBy fst

let test () =
    let solution = (dayTestInputs 7).[0] |> parse1 |> solve2
    printfn $"{solution}"

let part1 () =
    printfn $"Part 1: {dayInput 7 |> parse1 |> solve1}"

let part2 () =
    printfn $"Part 2: {dayInput 7 |> parse1 |> solve2}"
