module Day5

open AdventUtils
open System

let parse1 (input: string) =
    let [| ordering; updates |] = input.Split("\n\n")

    let parseOrderingRow (row: string) =
        let [| p1; p2 |] = row.Split("|") |> Array.map int
        (p1, p2)

    let parseUpdatesRow (row: string) =
        row.Split(",") |> Array.map int |> Array.toList

    let ordering = ordering.Split("\n") |> Array.map parseOrderingRow |> Array.toList
    let updates = updates.Split("\n") |> Array.map parseUpdatesRow |> Array.toList
    (ordering, updates)

let fNot f x = not (f x)

let solve1 (ordering, updates: int list list) =
    let isBefore x y =
        ordering
        |> List.filter (fun (before, _) -> before = x)
        |> List.exists (fun (_, after) -> after = y)

    let rec sorted (cmp: 'a -> 'a -> bool) list =
        match list with
        | [] -> []
        | (x :: xs) ->
            let before = List.filter (cmp x) xs
            let after = List.filter (fNot (cmp x)) xs
            (sorted (cmp) after) @ [ x ] @ (sorted (cmp) before)

    let isSorted cmp list = list = (sorted cmp list)

    let middle (list: 'a list) = list.[list.Length / 2]

    updates |> List.filter (isSorted (isBefore)) |> List.sumBy middle

let solve2 (ordering, updates: int list list) =
    let isBefore x y =
        ordering
        |> List.filter (fun (before, _) -> before = x)
        |> List.exists (fun (_, after) -> after = y)

    let rec sorted (cmp: 'a -> 'a -> bool) list =
        match list with
        | [] -> []
        | (x :: xs) ->
            let before = List.filter (cmp x) xs
            let after = List.filter (fNot (cmp x)) xs
            (sorted (cmp) after) @ [ x ] @ (sorted (cmp) before)

    let isSorted cmp list = list = (sorted cmp list)

    let middle (list: 'a list) = list.[list.Length / 2]

    updates
    |> List.filter (fNot (isSorted (isBefore)))
    |> List.sumBy (sorted (isBefore) >> middle)

let test () =
    let solution = (dayTestInputs 5).[0] |> parse1 |> solve2
    printfn $"{solution}"

let part1 () =
    printfn $"Part 1: {dayInput 5 |> parse1 |> solve1}"

let part2 () =
    printfn $"Part 2: {dayInput 5 |> parse1 |> solve2}"
