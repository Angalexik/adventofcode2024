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

let parse1 input =
    let (towels, seqs) = splitOnce "\n\n" input
    let towels = towels |> split ", " |> Array.toList
    let seqs = seqs |> split "\n"
    towels, seqs

let solve1 (towels, seqs) =
    seqs |> Array.filter (possible towels) |> Array.length

let solve2 input = ()

let test () =
    let solution = (dayTestInputs 19).[0] |> parse1 |> solve1
    printfn $"{solution}"

let part1 () =
    printfn $"Part 1: {dayInput 19 |> parse1 |> solve1}"

let part2 () =
    printfn $"Part 2: {dayInput 19 |> solve2}"
