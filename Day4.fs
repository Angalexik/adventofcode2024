module Day4

open AdventUtils
open System
open System.Text.RegularExpressions

// https://stackoverflow.com/q/3016139
let rec transpose =
    function
    | (_ :: _) :: _ as M -> List.map List.head M :: transpose (List.map List.tail M)
    | _ -> []

let transpose' a =
    a
    |> Array.map Array.toList
    |> Array.toList
    |> transpose
    |> List.toArray
    |> Array.map List.toArray

let diags (rows: char array array) =
    let height = rows.Length
    let width = rows.[0].Length

    let size = max height width |> (*) 2

    let newRows = Array.init size (fun _ -> Array.create size '.')

    for k = 0 to width + height - 2 do // Yes, this is me officialy giving up
        for j = 0 to k do
            let i = k - j

            if i < height && j < width then
                newRows.[k].[j] <- rows.[i].[j]

    newRows

let flatten a2d =
    [| 0 .. Array2D.length1 a2d - 1 |]
    |> Array.map (fun y -> a2d[y, *])
    |> Array.fold (Array.append) [||]

let parse1 (input: string) =
    let chars = input |> _.Split("\n") |> Array.map _.ToCharArray()
    chars

let parse2 (rows: char array array) =
    Array2D.init rows.Length rows.[0].Length (fun y x -> rows.[y].[x])

let solve1 input =
    let horizontalMatches (rows: char array array) =
        rows
        |> Array.map (
            (fun (r: char array) -> new String(r)) >> (fun r -> Regex.Count(r, "XMAS"))
        )
        |> Array.sum

    let revRows = Array.map Array.rev

    [
        input |> horizontalMatches
        input |> revRows |> horizontalMatches
        input |> transpose' |> horizontalMatches
        input |> transpose' |> revRows |> horizontalMatches
        input |> diags |> horizontalMatches
        input |> diags |> revRows |> horizontalMatches
        input |> revRows |> diags |> horizontalMatches
        input |> revRows |> diags |> revRows |> horizontalMatches
    ]
    |> List.sum

let solve2 (input: char array2d) =
    let tryGet y x =
        if
            List.contains y [ 0 .. (Array2D.length1 input) - 1 ]
            && List.contains x [ 0 .. (Array2D.length2 input) - 1 ]
        then
            input[y, x]
        else
            '.'

    let isXmas y x =
        let acceptable = [ 'M'; 'S' ]
        let diag1neighbour1 = tryGet (y + 1) (x + 1)
        let diag1neighbour2 = tryGet (y - 1) (x - 1)
        let diag2neighbour1 = tryGet (y - 1) (x + 1)
        let diag2neighbour2 = tryGet (y + 1) (x - 1)

        [ diag1neighbour1; diag1neighbour2; diag2neighbour1; diag2neighbour2 ]
        |> List.forall (fun n -> List.contains n acceptable)
        && diag1neighbour1 <> diag1neighbour2
        && diag2neighbour1 <> diag2neighbour2
        && (diag1neighbour1 = diag2neighbour1 || diag1neighbour1 = diag2neighbour2)
        && (diag2neighbour1 = diag1neighbour2 || diag2neighbour1 = diag1neighbour1)
        && (diag1neighbour2 = diag2neighbour2 || diag1neighbour2 = diag2neighbour1)

    input
    |> Array2D.mapi (fun y x element -> if element = 'A' then isXmas y x else false)
    |> flatten
    |> Array.filter id
    |> Array.length


let test () =
    let solution = (dayTestInputs 4).[4] |> parse1 |> parse2 |> solve2
    printfn $"{solution}"

let part1 () =
    printfn $"Part 1: {dayInput 4 |> parse1 |> solve1}"

let part2 () =
    printfn $"Part 2: {dayInput 4 |> parse1 |> parse2 |> solve2}"
