module Day3

open AdventUtils
open System
open System.Text.RegularExpressions

type Mul = { Index: int; Num1: int; Num2: int }

type Instrution =
    | Do of int
    | Dont of int
    | Mul of Mul

let index =
    function
    | Do(idx) -> idx
    | Dont(idx) -> idx
    | Mul({ Index = idx }) -> idx

let parse1 input =
    Regex.Matches(input, @"mul\((?<num1>\d{1,3}),(?<num2>\d{1,3})\)")
    |> Seq.map (fun matsch -> (matsch.Groups.[1].Value |> int, matsch.Groups[2].Value |> int))

let parse2 input =
    let dontIdxs = Regex.Matches(input, @"don't\(\)") |> Seq.map (_.Index >> Dont)
    let doIdxs = Regex.Matches(input, @"do\(\)") |> Seq.map (_.Index >> Do)

    let muls =
        Regex.Matches(input, @"mul\((?<num1>\d{1,3}),(?<num2>\d{1,3})\)")
        |> Seq.map (
            (fun matsch ->
                { Index = matsch.Index
                  Num1 = matsch.Groups.[1].Value |> int
                  Num2 = matsch.Groups.[2].Value |> int })
            >> Mul
        )

    dontIdxs |> Seq.append doIdxs |> Seq.append muls |> Seq.sortBy index

let solve1 input =
    input |> Seq.sumBy (fun (num1, num2) -> num1 * num2)

let solve2 instructions =
    let folder (enabled, total) curr =
        match curr with
        | Do(_) -> (true, total)
        | Dont(_) -> (false, total)
        | Mul({ Num1 = n1; Num2 = n2 }) -> (enabled, if enabled then total + (n1 * n2) else total)

    instructions |> Seq.fold folder (true, 0) |> snd

let test () =
    let solution = (dayTestInputs 3).[1] |> parse2 |> solve2
    printfn $"{solution}"

let part1 () =
    printfn $"Part 1: {dayInput 3 |> parse1 |> solve1}"

let part2 () =
    printfn $"Part 2: {dayInput 3 |> parse2 |> solve2}"
