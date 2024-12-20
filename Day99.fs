// Actually Advent 2023 Day 1
module Day99

open AdventUtils
open System

let numbers =
    Map
        [
            ("one", "1")
            ("two", "2")
            ("three", "3")
            ("four", "4")
            ("five", "5")
            ("six", "6")
            ("seven", "7")
            ("eight", "8")
            ("nine", "9")
        ]

// This took me 3 and 1/2 hours by the way
let stuff cmp concatenate indexOf init line =
    let fallibleIndexOf a =
        indexOf a >> (fun idx -> if idx = -1 then None else Some(idx))

    let indexOfDigit =
        Seq.choose (fallibleIndexOf line) numbers.Values
        |> Seq.fold (fun acc idx -> if cmp acc idx then acc else idx) init

    let (indexOfWord, word) =
        Seq.choose
            (fun (word: string) ->
                match fallibleIndexOf line word with
                | None -> None
                | Some(index) -> Some(index, word))
            numbers.Keys
        |> Seq.fold
            (fun acc curr -> if cmp (fst acc) (fst curr) then acc else curr)
            (init, "")

    if cmp indexOfWord indexOfDigit then
        concatenate numbers.[word] line
    else
        line

let ifFirstThenPrepend =
    stuff (<) (+) (fun container value -> container.IndexOf(value)) 1000

let ifLastThenAppend =
    stuff (>) (fun x y -> y + x) (fun container value -> container.LastIndexOf(value)) -1

let solve1 (input: string) =
    input.Split '\n'
    |> Array.map (fun line ->
        line.ToCharArray()
        |> Array.filter Char.IsDigit
        |> (fun chars -> String.Join("", [ Seq.head chars; Seq.last chars ]) |> int))
    |> Array.sum

let solve2 (input: string) =
    input.Split '\n'
    |> Array.map (fun line ->
        ifFirstThenPrepend line
        |> ifLastThenAppend
        |> _.ToCharArray()
        |> Array.filter Char.IsDigit
        |> (fun chars -> String.Join("", [ Seq.head chars; Seq.last chars ]) |> int))
    |> Array.sum

let test () =
    let solution = solve2 (dayTestInputs 99).[1]
    printfn $"{solution}"

let part1 () =
    printfn $"Part 1: {solve1 <| dayInput 99}"

let part2 () =
    printfn $"Part 2: {solve2 <| dayInput 99}"
