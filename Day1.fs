module Day1

open AdventUtils
open System

let parse1 (text: string) =
    let rec parseLines (l1, l2) (lines: string list) =
        match lines with
        | [] -> (l1, l2)
        | (line :: ls) ->
            let (e1, e2) =
                line.Split("   ")
                |> Array.toList
                |> List.map int
                |> List.take 2
                |> function
                    | [ e1; e2 ] -> (e1, e2)
                    | _ -> failwith "Unreachable!"

            parseLines (e1 :: l1, e2 :: l2) ls

    text.Split('\n') |> Array.toList |> parseLines ([], [])

let solve1 (list1, list2) =
    let sorted1 = List.sort list1
    let sorted2 = List.sort list2

    List.zip sorted1 sorted2
    |> List.sumBy (fun ((e1, e2): int * int) -> Math.Abs(e1 - e2))

let solve2 (list1, list2) =
    let rec occurences num list e =
        match list with
        | [] -> num
        | (x :: xs) -> occurences (if e = x then num + 1 else num) xs e

    List.sumBy (fun e1 -> e1 * occurences 0 list2 e1) list1

let test () =
    let solution = (dayTestInputs 1).[0] |> parse1 |> solve2
    printfn $"{solution}"

let part1 () =
    printfn $"Part 1: {dayInput 1 |> parse1 |> solve1}"

let part2 () =
    printfn $"Part 2: {dayInput 1 |> parse1 |> solve2}"
