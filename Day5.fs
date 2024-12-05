module Day5

open AdventUtils
open System
open Utils.Text

// https://wiki.haskell.org/Introduction#Quicksort_in_Haskell
let rec sorted (cmp: 'a -> 'a -> bool) list =
    match list with
    | [] -> []
    | (x :: xs) ->
        let before = List.filter (cmp x) xs
        let after = List.filter ((cmp x) >> not) xs
        (sorted (cmp) after) @ [ x ] @ (sorted (cmp) before)

let isSorted cmp list = list = (sorted cmp list)

let middle (list: 'a list) = list.[list.Length / 2]

let isBefore ordering x y =
    ordering
    |> List.filter (fun (before, _) -> before = x)
    |> List.exists (fun (_, after) -> after = y)

let parse1 (input: string) =
    let ordering, updates = splitOnce "\n\n" input

    let parseOrderingRow (row: string) =
        splitOnce "|" row |> (fun (x, y) -> (int x, int y))

    let parseUpdatesRow (row: string) =
        row.Split(",") |> Array.map int |> Array.toList

    let ordering = ordering.Split("\n") |> Array.map parseOrderingRow |> Array.toList
    let updates = updates.Split("\n") |> Array.map parseUpdatesRow |> Array.toList
    (ordering, updates)

let solve1 (ordering, updates: int list list) =
    let isBefore = isBefore ordering

    updates |> List.filter (isSorted isBefore) |> List.sumBy middle

let solve2 (ordering, updates: int list list) =
    let isBefore = isBefore ordering

    updates
    |> List.filter (isSorted (isBefore) >> not)
    |> List.sumBy (sorted (isBefore) >> middle)

let test () =
    let solution = (dayTestInputs 5).[0] |> parse1 |> solve2
    printfn $"{solution}"

let part1 () =
    printfn $"Part 1: {dayInput 5 |> parse1 |> solve1}"

let part2 () =
    printfn $"Part 2: {dayInput 5 |> parse1 |> solve2}"
