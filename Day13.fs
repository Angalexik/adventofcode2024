module Day13

open AdventUtils
open System
open System.Text.RegularExpressions
open Utils.Text

let (--) (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

let (+^) x y =
    let res = x + y
    if res < x || res < y then UInt32.MaxValue else res

type Button =
    | A
    | B

let price =
    function
    | A -> 3
    | B -> 1

type Machine =
    { A: int * int
      B: int * int
      Target: int * int }

let machine [ ax; ay; bx; by; tx; ty ] =
    { A = (ax, ay)
      B = (bx, by)
      Target = (tx, ty) }

let consequence (machine: Machine) button =
    match button with
    | A -> machine.A
    | B -> machine.B

let pointPrice machine point =
    let mutable memo = Map.empty
    let consequence = consequence machine

    // let rec loop2 position aPresses bPresses =
    //     match position, aPresses, bPresses with
    //     | (_, 0, _) | (_, _, 0) -> Int32

    let rec loop position aPresses bPresses button =
        let key = (position, aPresses, bPresses, button)

        match position, aPresses, bPresses with
        | ((0, 0), _, _) -> 0u
        | (_, 0, _)
        | (_, _, 0) -> UInt32.MaxValue
        | ((x, y), _, _) when x < 0 || y < 0 -> UInt32.MaxValue
        | _ ->
            match Map.tryFind key memo with
            | Some(cached) -> cached
            | None ->
                let price = price button |> uint

                let partial =
                    loop
                        (position -- (consequence button))
                        (if button = A then aPresses - 1 else aPresses)
                        (if button = B then bPresses - 1 else bPresses)

                let result = (partial A, partial B) ||> min |> (+^) price
                memo <- Map.add key result memo
                result

    let partial = loop point 101 101
    (partial A, partial B) ||> min

let parse1 input =
    let regex =
        Regex @"Button A: X\+(\d+), Y\+(\d+)\nButton B: X\+(\d+), Y\+(\d+)\nPrize: X=(\d+), Y=(\d+)"

    input
    |> split "\n\n"
    |> Array.map (
        regex.Match
        >> _.Groups
        >> Seq.toList
        >> List.tail
        >> (List.map (_.Value >> int))
        >> machine
    )

let solve1 (input: Machine array) =
    input
    |> Array.choose (fun machine ->
        match pointPrice machine machine.Target with
        | UInt32.MaxValue -> None
        | n -> Some n)
    |> Array.sum

let solve2 input = ()

let test () =
    let solution = (dayTestInputs 13).[0] |> parse1 |> solve1
    printfn "%A" solution

let part1 () =
    printfn $"Part 1: {dayInput 13 |> parse1 |> solve1}"

let part2 () =
    printfn $"Part 2: {dayInput 13 |> solve2}"
