module Day2

open AdventUtils
open System

let processReport (report: string array) =
    let rec isValid (valid: bool) decreasing prev data =
        match valid with
        | false -> false
        | true ->
            match data with
            | [] -> valid
            | (x :: xs) ->
                match prev with
                | None -> isValid valid None (Some x) xs
                | Some(prev) ->
                    let diff = prev - x

                    let currentlyDecreasing =
                        match diff with
                        | neg when neg < 0 -> false
                        | pos when pos > 0 -> true
                        | 0 -> true // doesn't really matter

                    match decreasing with
                    | None -> isValid (diff <> 0 && (Math.Abs diff <= 3)) (Some currentlyDecreasing) (Some x) xs
                    | Some(decreasing) ->
                        isValid
                            (diff <> 0 && (Math.Abs diff <= 3) && decreasing = currentlyDecreasing)
                            (Some decreasing)
                            (Some x)
                            xs

    report |> Array.map int |> Array.toList |> isValid true None None

let removeElement at array =
    array
    |> Array.mapi (fun i e -> (i <> at, e))
    |> Array.choose (fun (keep, e) -> if keep then Some e else None)

let parse =
    (fun (t: string) -> t.Split('\n')) >> Array.map (fun (l: string) -> l.Split(" "))

let solve1 input =
    input |> Array.filter processReport |> _.Length

let solve2 input =
    let isSafeWithRemoved report =
        let count = Array.length report
        Array.replicate count report |> Array.mapi removeElement |> Array.exists processReport
    let either f g x = f x || g x
    input |> Array.filter (either processReport isSafeWithRemoved) |> _.Length

let test () =
    let solution = (dayTestInputs 2).[0] |> parse |> solve2
    printfn $"{solution}"

let part1 () =
    printfn $"Part 1: {dayInput 2 |> parse |> solve1}"

let part2 () =
    printfn $"Part 2: {dayInput 2 |> parse |> solve2}"
