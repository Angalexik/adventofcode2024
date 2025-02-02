module Day19

open AdventUtils
open System
open Utils.Text

let possible towels target =
    let rec loop queue seen =
        match queue with
        | [] -> false
        | (x :: xs) ->
            if x = "" then
                true
            elif Set.contains x seen then
                loop xs seen
            else
                let noEnd =
                    towels
                    |> List.choose (fun (t: string) ->
                        if x.EndsWith(t) then
                            Some <| x.Substring(0, x.Length - t.Length)
                        else
                            None)

                loop (xs @ noEnd) (Set.add x seen)

    loop [ target ] Set.empty

// Absolutely incomprehensible
let ways towels target =
    let mutable cache = Map.ofList [ ("", 0UL) ]

    let rec loop (partial: string) =
        match cache.TryFind partial with
        | Some v -> v
        | None ->
            let plusOne =
                if towels |> List.exists (fun e -> e = partial) then
                    1UL
                else
                    0UL

            let noEnd =
                towels
                |> List.choose (fun (t: string) ->
                    if partial.EndsWith(t) then
                        Some <| partial.Substring(0, partial.Length - t.Length)
                    else
                        None)

            let ret = noEnd |> List.map loop |> List.sum |> (+) plusOne
            cache <- Map.add partial ret cache
            ret

    loop target

let parse1 input =
    let (towels, seqs) = splitOnce "\n\n" input
    let towels = towels |> split ", " |> Array.toList
    let seqs = seqs |> split "\n"
    towels, seqs

let solve1 (towels, seqs) =
    seqs |> Array.filter (possible towels) |> Array.length

let solve2 (towels, seqs) =
    seqs |> Array.map (ways towels) |> Array.sum

let test () =
    let solution = (dayTestInputs 19).[0] |> parse1 |> solve2
    printfn "%A" solution

let part1 () =
    printfn $"Part 1: {dayInput 19 |> parse1 |> solve1}"

let part2 () =
    printfn $"Part 2: {dayInput 19 |> parse1 |> solve2}"
