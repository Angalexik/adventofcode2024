module Day12

open AdventUtils
open System
open Utils.Func
open Utils.Array

let (++) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

let tryGet a (y, x) = (Array2D.get a y |> tryF) x

let neighbours grid pos marker =
    let offsets = [ (1, 0); (-1, 0); (0, 1); (0, -1) ]

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

let regionSides (region: bool array2d) =
    let rows = Array2D.length1 region + 1
    let columns = Array2D.length2 region + 1
    let mutable sides = 0

    for y = -1 to rows do
        for x = -1 to columns do
            let realY = y //* 2
            let realX = x //* 2

            let diag1 = [ (realY, realX); (realY + 1, realX + 1) ]
            let diag2 = [ (realY + 1, realX); (realY, realX + 1) ]

            let cells =
                diag1 @ diag2
                // |> dbg
                |> List.choose (fun pos -> tryGet region pos |> Option.bind (fun e -> if e then Some pos else None))
            // |> dbg

            if cells.Length % 2 = 1 then
                sides <- sides + 1
            elif cells = diag1 || cells = diag2 then
                sides <- sides + 2

    sides

let solve1 input =
    Array2D.mapi (fun y x _ -> (y, x)) input
    |> flat2Darray
    |> Seq.fold
        (fun (visited, price) curr ->
            if List.contains curr visited then
                (visited, price)
            else
                let (currVisited, currArea, currPerimiter) = regionPriceAndElements input curr
                let currPrice = currArea * currPerimiter
                (visited @ currVisited, price + currPrice))
        ([], 0)
    |> snd

let solve2 input =
    let positionsToArray2d positions =
        Array2D.init (Array2D.length1 input) (Array2D.length2 input) (fun y x -> List.contains (y, x) positions)

    let regions =
        Array2D.mapi (fun y x _ -> (y, x)) input
        |> flat2Darray
        |> Seq.fold
            (fun regions curr ->
                if List.exists (List.contains curr) regions then
                    regions
                else
                    let (region, _, _) = regionPriceAndElements input curr
                    region :: regions)
            []
        |> List.sumBy (fun positions -> positions |> positionsToArray2d |> regionSides |> (*) positions.Length)
        // |> List.map (positionsToArray2d >> regionSides)

    regions

let test () =
    let parsed = (dayTestInputs 12).[3] |> charGrid
    let solution = parsed |> solve2 |> dbg
    printfn $"{solution}"

let part1 () =
    printfn $"Part 1: {dayInput 12 |> charGrid |> solve1}"

let part2 () =
    printfn $"Part 2: {dayInput 12 |> charGrid |> solve2}"
