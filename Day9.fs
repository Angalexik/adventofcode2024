module Day9

open AdventUtils
open System
open Utils.Text
open ShellProgressBar

type Id =
    | Empty
    | Id of int64

let idxToId (idx: int64) =
    if idx % 2L = 0L then idx / 2L |> Id else Empty

let isFile id =
    match id with
    | Empty -> false
    | Id(_) -> true

let checkSum list =
    list
    |> List.indexed
    |> List.sumBy (function
        | (i, Id(id)) -> id * int64 i
        | (_, Empty) -> failwith "Unreachable?")

let checkSum' array =
    array
    |> Array.indexed
    |> Array.sumBy (function
        | (i, Id(id)) -> id * int64 i
        | (_, Empty) -> 0)

let toString (array: (int * Id) array) =
    let idToStr id =
        match id with
        | Empty -> '.'
        | Id(id) -> id.ToString().[0]

    let expand (length, id) = Array.replicate length (idToStr id)
    array |> Array.collect expand |> (fun s -> new String(s))

let parse1 input =
    use pbar = new ProgressBar(19999, "Parsing")
    let expand (length, id) = List.replicate length id

    input
    |> chars
    |> Array.mapi (fun i e -> (charToInt e, idxToId i))
    |> Array.fold
        (fun acc curr ->
            pbar.Tick()
            acc @ (expand curr))
        []

let parse2 input =
    input |> chars |> Array.mapi (fun i e -> (charToInt e, idxToId i))

let solve1 input =
    printfn "Solving!"
    let empties = List.filter ((=) Empty) input |> List.length
    use pbar = new ProgressBar(empties, "Solving")

    // Maybe slow because I'm also moving empty spaces at the end
    let rec loop list =
        if not <| List.exists ((=) Empty) list then
            list
        else
            pbar.Tick()
            let emptyIdx = List.findIndex ((=) Empty) list
            let list = List.updateAt emptyIdx (List.last list) list
            List.truncate (List.length list - 1) list |> loop

    loop input |> checkSum
// let moveLastFileBlockToFirstEmptySpace list =
//     let emptyIndex = List.findIndex ((=) Empty) list
//     let fileBlockIndex = List.findIndexBack isFile list

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
            // let (fileSize, id) = dbg array.[fileIdx]
            let (fileSize, id) = array.[fileIdx]

            let spaceIdx =
                Array.tryFindIndex (fun (size, id) -> id = Empty && size >= fileSize) array
                // |> dbg

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

                // toString array |> dbg |> ignore

                loop (fileIdx - 1) array

    let expand (length, id) = Array.replicate length id

    // let firstSpaceIdx = Array.findIndex (snd >> ((=) Empty)) input
    let lastFileIdx = Array.findIndexBack (snd >> isFile) input
    loop lastFileIdx input |> Array.collect expand |> checkSum'
// let rec loop spaceId spaceSize array =
//     let popped = Array.truncate (Array.length array - 1) array

//     match Array.last array with
//     | (_, Empty) -> loop spaceId spaceSize popped
//     | (size, Id(id)) ->
//         if size <= spaceSize then
//             let nextSpaceId = Array.skip
//             if size = spaceSize then
//                 let array = Array.updateAt spaceId (size, Id id) popped
//                 loop spaceId spaceSize array

let test () =
    let solution = (dayTestInputs 9).[0] |> parse2 |> solve2
    printfn "%A" solution

let part1 () =
    printfn $"Part 1: {dayInput 9 |> parse1 |> solve1}"

let part2 () =
    printfn $"Part 2: {dayInput 9 |> parse2 |> solve2}"
