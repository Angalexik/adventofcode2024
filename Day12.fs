module Day12

open AdventUtils
open System
open Utils.Func
open Utils.Array

let (++) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

let neighbours grid pos marker =
    let offsets = [ (1, 0); (-1, 0); (0, 1); (0, -1) ]
    let tryGet a (y, x) = (Array2D.get a y |> tryF) x

    offsets
    |> List.choose (fun offset ->
        let newPos = pos ++ offset
        newPos
        |> tryGet grid
        |> Option.bind (fun x -> if x = marker then Some newPos else None))

let regionPriceAndElements grid start =
    let rec loop area perimiter visited toVisit =
        match toVisit with
        | [] -> visited, area, perimiter
        | (x :: xs) ->
            if List.contains x visited then
                loop area perimiter visited xs
            else
                let neighbouring = neighbours grid x grid[fst x, snd x]
                let perimiter = perimiter + (4 - neighbouring.Length)
                loop (area + 1) perimiter (x :: visited) (neighbouring @ xs)

    loop 0 0 [] [ start ]

let allRegions grid =
    Array2D.mapi (fun y x _ -> (y, x)) grid
    |> flat2Darray
    |> Seq.fold
        (fun (visited, price) curr ->
            if List.contains curr visited then
                (visited, price)
            else
                let (currVisited, currArea, currPerimiter) = regionPriceAndElements grid curr
                let currPrice = currArea * currPerimiter
                (visited @ currVisited, price + currPrice))
        ([], 0)
    |> snd

let solve1 input = allRegions input

let solve2 input = ()

let test () =
    let parsed = (dayTestInputs 12).[0] |> charGrid
    dbg <| regionPriceAndElements parsed (0, 0)
    let solution = parsed |> solve1
    printfn $"{solution}"

let part1 () =
    printfn $"Part 1: {dayInput 12 |> charGrid |> solve1}"

let part2 () =
    printfn $"Part 2: {dayInput 12 |> solve2}"
