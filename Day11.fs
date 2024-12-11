module Day11

open AdventUtils
open System
open Utils.Text
open Utils.Func

type Bag<'a when 'a: comparison> = Map<'a, uint64>

module Bag =
    let empty: 'a Bag = Map.empty

    let isEmpty (bag: 'a Bag) = bag = Map.empty

    let addMany value count (bag: 'a Bag) : 'a Bag =
        if count < 0UL then
            invalidArg "count" "Must not be a negative nmber"

        Map.change
            value
            (fun old ->
                match old with
                | None -> Some(uint64 count)
                | Some(oldCount) -> Some(uint64 count + oldCount))
            bag

    let removeMany value count (bag: 'a Bag) : 'a Bag =
        if count < 0UL then
            invalidArg "count" "Must not be a negative nmber"

        Map.change
            value
            (Option.bind (fun oldCount ->
                if oldCount <= uint64 count then
                    None
                else
                    Some(oldCount - uint64 count)))
            bag

    let add value (bag: 'a Bag) : 'a Bag = addMany value 1UL bag

    let remove value (bag: 'a Bag) : 'a Bag = removeMany value 1UL bag

    let count (bag: 'a Bag) = Map.values bag |> Seq.sum

    let fromSeq (seq: 'a seq) =
        Seq.fold (fun acc curr -> add curr acc) empty seq

let applyRules (stones: int64 Bag) =
    let splitNum number =
        let str = number.ToString()
        let length = str.Length / 2
        let firstHalf = str.Substring(0, length) |> int64
        let secondHalf = str.Substring(length) |> int64
        (firstHalf, secondHalf)

    stones
    |> Map.toSeq
    |> Seq.fold
        (fun acc (stone, count) ->
            acc
            |> match stone with
               | 0L -> Bag.addMany 1L count
               | even when (stone.ToString().Length % 2 = 0) ->
                   let (firstStone, secondStone) = splitNum even
                   Bag.addMany firstStone count >> Bag.addMany secondStone count
               | n -> Bag.addMany (n * 2024L) count)
        Bag.empty

let parse1 input =
    split " " input |> Array.map int64 |> Bag.fromSeq

let solve1 input =
    (repeat 25 applyRules) input |> Bag.count

let solve2 input =
    (repeat 75 applyRules) input |> Bag.count

let test () =
    let inputs = (dayTestInputs 11)
    let test1 = inputs.[0] |> parse1 |> applyRules
    dbg test1 |> ignore
    let test2 = inputs.[1] |> parse1 |> (repeat 6 applyRules)
    dbg test2 |> ignore

let part1 () =
    printfn $"Part 1: {dayInput 11 |> parse1 |> solve1}"

let part2 () =
    printfn $"Part 2: {dayInput 11 |> parse1 |> solve2}"
