module Day3

open AdventUtils
open System
open System.Text.RegularExpressions

type Mul = { Num1: int; Num2: int }

type Instrution =
    | Do
    | Dont
    | Mul of Mul

let parse1 input =
    Regex.Matches(input, @"mul\((?<num1>\d{1,3}),(?<num2>\d{1,3})\)")
    |> Seq.map (fun matsch ->
        (matsch.Groups.[1].Value |> int, matsch.Groups[2].Value |> int))

let parse2 input =
    let toInstrution (matsch: Match) =
        match matsch.Value with
        | "do()" -> Do
        | "don't()" -> Dont
        | _ ->
            Mul(
                {
                    Num1 = matsch.Groups.[1].Value |> int
                    Num2 = matsch.Groups.[2].Value |> int
                }
            )

    Regex.Matches(input, @"mul\((?<num1>\d{1,3}),(?<num2>\d{1,3})\)|do\(\)|don't\(\)")
    |> Seq.map toInstrution

let solve1 input =
    input |> Seq.sumBy (fun (num1, num2) -> num1 * num2)

let solve2 instructions =
    let folder (enabled, total) curr =
        match curr with
        | Do -> (true, total)
        | Dont -> (false, total)
        | Mul({ Num1 = n1; Num2 = n2 }) ->
            (enabled, if enabled then total + (n1 * n2) else total)

    instructions |> Seq.fold folder (true, 0) |> snd

let test () =
    let solution = (dayTestInputs 3).[1] |> parse2 |> solve2
    printfn $"{solution}"

let part1 () =
    printfn $"Part 1: {dayInput 3 |> parse1 |> solve1}"

let part2 () =
    printfn $"Part 2: {dayInput 3 |> parse2 |> solve2}"
