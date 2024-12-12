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

    let fromSeq (seq: 'a seq) : 'a Bag =
        Seq.fold (fun acc curr -> add curr acc) empty seq

    let fromOccurenceSeq (seq: ('a * uint64) seq) : 'a Bag =
        Seq.fold (fun acc (currValue, currCount) -> addMany currValue currCount acc) empty seq

    let toOccurenceSeq (bag: 'a Bag) = Map.toSeq bag

    let count (bag: 'a Bag) = Map.values bag |> Seq.sum

    let union (bag1: 'a Bag) (bag2: 'a Bag) : 'a Bag =
        toOccurenceSeq bag1
        |> Seq.fold (fun acc (value, count) -> addMany value count acc) bag2

    let collect (mapping: 'a -> 'b Bag) (bag: 'a Bag) : 'b Bag =
        let timesF =
            function
            | (value, 1UL) -> mapping value
            | (value, many) -> mapping value |> Map.map (fun _ c -> c * many)

        toOccurenceSeq bag |> Seq.collect (timesF >> toOccurenceSeq) |> fromOccurenceSeq

    let singleton value = Map.empty |> add value

let applyRules (stones: int64 Bag) =
    let splitNum number =
        let str = number.ToString()
        let length = str.Length / 2
        let firstHalf = str.Substring(0, length) |> int64
        let secondHalf = str.Substring(length) |> int64
        (Bag.singleton firstHalf, Bag.singleton secondHalf) ||> Bag.union

    stones
    |> Bag.collect (fun stone ->
        match stone with
        | 0L -> Bag.singleton 1L
        | even when (stone.ToString().Length % 2 = 0) -> splitNum even
        | n -> Bag.singleton (n * 2024L))

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
