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

let checkSum seq =
    seq
    |> Seq.indexed
    |> Seq.sumBy (function
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
        let emptyIdx = List.tryFindIndex ((=) Empty) list

        match emptyIdx with
        | None -> list
        | Some(idx) ->
            pbar.Tick()
            let list = List.updateAt idx (List.last list) list
            List.truncate (List.length list - 1) list |> loop

    input |> loop |> checkSum

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

    let expand (length, id) = Array.replicate length id

    let lastFileIdx = Array.findIndexBack (snd >> isFile) input
    loop lastFileIdx input |> Array.collect expand |> checkSum

let test () =
    let solution = (dayTestInputs 9).[0] |> parse1 |> solve1
    printfn "%A" solution
    let solution = (dayTestInputs 9).[0] |> parse2 |> solve2
    printfn "%A" solution

let part1 () =
    printfn $"Part 1: {dayInput 9 |> parse1 |> solve1}"

let part2 () =
    printfn $"Part 2: {dayInput 9 |> parse2 |> solve2}"
