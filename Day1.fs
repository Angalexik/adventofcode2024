module Day1

open AdventUtils
open System

let parse1 (text: string) =
    let parseLines' =
        List.fold
            (fun (l1, l2) (line: string) ->
                let (e1, e2) =
                    line.Split("   ")
                    |> Array.toList
                    |> List.map int
                    |> List.take 2
                    |> function
                        | [ e1; e2 ] -> (e1, e2)
                        | _ -> failwith "Unreachable!"

                (e1 :: l1, e2 :: l2))
            ([], [])

    text.Split('\n') |> Array.toList |> parseLines'

let solve1 (list1, list2) =
    let sorted1 = List.sort list1
    let sorted2 = List.sort list2

    List.zip sorted1 sorted2
    |> List.sumBy (fun ((e1, e2): int * int) -> Math.Abs(e1 - e2))

let solve2 (list1, list2) =
    let occurences list e =
        List.fold (fun acc curr -> if curr = e then acc + 1 else acc) 0 list

    List.sumBy (fun e1 -> e1 * occurences list2 e1) list1

let test () =
    let solution = (dayTestInputs 1).[0] |> parse1 |> solve2
    printfn $"{solution}"

let part1 () =
    printfn $"Part 1: {dayInput 1 |> parse1 |> solve1}"

let part2 () =
    printfn $"Part 2: {dayInput 1 |> parse1 |> solve2}"
