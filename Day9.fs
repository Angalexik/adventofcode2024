module Day9

open AdventUtils
open System
open Utils.Text

type Id =
    | Empty
    | Id of int64

let idxToId (idx: int64) =
    if idx % 2L = 0L then idx / 2L |> Id else Empty

let isFile id =
    match id with
    | Empty -> false
    | Id(_) -> true

let checkSum seq =
    seq
    |> Seq.indexed
    |> Seq.sumBy (function
        | (i, Id(id)) -> id * int64 i
        | (_, Empty) -> 0)

let expand (length, id) = Array.replicate length id

let parse1 input =
    input
    |> chars
    |> Array.mapi (fun i e -> (charToInt e, idxToId i))
    |> Array.collect (fun curr -> expand curr)

let parse2 input =
    input |> chars |> Array.mapi (fun i e -> (charToInt e, idxToId i))

let solve1 input =
    // Extremely hot loop
    let rec loop fileIdx (array: Id array) =
        if array.[fileIdx] = Empty then
            loop (fileIdx - 1) array
        else
            let emptyIdx = Array.findIndex ((=) Empty) array

            match emptyIdx with
            | idx when idx > fileIdx -> array
            | idx ->
                array.[idx] <- array.[fileIdx]
                array.[fileIdx] <- Empty

                array |> loop (fileIdx - 1)

    input |> loop (input.Length - 1) |> checkSum

// Probably slow
let insertAt idx elem array =
    let before = Array.truncate idx array
    let after = Array.skip idx array
    Array.concat [ before; [| elem |]; after ]

let solve2 input =
    let rec loop fileIdx (array: (int * Id) array) =
        if fileIdx = 0 then
            array
        elif snd array.[fileIdx] = Empty then
            loop (fileIdx - 1) array
        else
            let (fileSize, id) = array.[fileIdx]

            let spaceIdx =
                Array.tryFindIndex (fun (size, id) -> id = Empty && size >= fileSize) array

            match spaceIdx with
            | None -> loop (fileIdx - 1) array
            | Some(spaceIdx) when spaceIdx > fileIdx -> loop (fileIdx - 1) array
            | Some(spaceIdx) ->
                let (spaceSize, _) = array.[spaceIdx]

                let array =
                    array
                    |> Array.updateAt spaceIdx (spaceSize - fileSize, Empty)
                    |> Array.updateAt fileIdx (fileSize, Empty)
                    |> insertAt spaceIdx (fileSize, id)

                loop (fileIdx - 1) array

    let lastFileIdx = Array.findIndexBack (snd >> isFile) input
    loop lastFileIdx input |> Array.collect expand |> checkSum

let test () =
    let solution = (dayTestInputs 9).[1] |> parse1 |> solve1
    printfn "%A" solution
    let solution = (dayTestInputs 9).[1] |> parse2 |> solve2
    printfn "%A" solution

let part1 () =
    printfn $"Part 1: {dayInput 9 |> parse1 |> solve1}"

let part2 () =
    printfn $"Part 2: {dayInput 9 |> parse2 |> solve2}"
