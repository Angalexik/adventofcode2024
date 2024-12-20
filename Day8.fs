module Day8

open AdventUtils
open System
open Utils.Text

let (++) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

let (--) (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

// https://stackoverflow.com/a/1231711
let rec comb n l =
    match n, l with
    | 0, _ -> [ [] ]
    | _, [] -> []
    | k, (x :: xs) -> List.map ((@) [ x ]) (comb (k - 1) xs) @ comb k xs

let inBounds (bounds: int * int) (pos: int * int) =
    List.contains (fst pos) [ 0 .. fst bounds - 1 ]
    && List.contains (snd pos) [ 0 .. snd bounds - 1 ]

let parse1 input =
    let rows = input |> split "\n"
    let grid = Array2D.init rows.Length rows.[0].Length (fun y x -> rows.[y].[x])
    let mutable frequencies = Map.empty

    Array2D.iteri
        (fun y x e ->
            if Char.IsLetterOrDigit(e) then
                frequencies <-
                    Map.change
                        e
                        (fun coords -> (x, y) :: (Option.defaultValue [] coords) |> Some)
                        frequencies)
        grid

    // Everything in x, y format
    frequencies, (Array2D.length2 grid, Array2D.length1 grid)

let solve1 (frequencies, bounds) =
    let antinodesForFreq _ antennas =
        antennas
        |> comb 2
        |> List.collect (fun [ a1; a2 ] -> [ a1 ++ (a1 -- a2); a2 ++ (a2 -- a1) ])
        |> List.filter (inBounds bounds)

    frequencies
    |> Map.map antinodesForFreq
    |> Seq.collect _.Value
    |> Set.ofSeq
    |> Set.count

let solve2 (frequencies, bounds) =
    let antinodesForPair (a1, a2) =
        let rec loop nodes pos dir =
            if inBounds bounds pos then
                loop (pos :: nodes) (pos ++ dir) dir
            else
                nodes

        let loop = loop []
        a1 :: a2 :: (loop a1 (a1 -- a2)) @ (loop a1 (a2 -- a1))

    let antinodesForFreq _ antennas =
        antennas
        |> comb 2
        |> List.collect (fun [ a1; a2 ] -> antinodesForPair (a1, a2))
        |> List.filter (inBounds bounds)

    frequencies
    |> Map.map antinodesForFreq
    |> Seq.collect _.Value
    |> Set.ofSeq
    |> Set.count

let test () =
    let solution = (dayTestInputs 8).[0] |> parse1 |> solve2
    printfn $"{solution}"

let part1 () =
    printfn $"Part 1: {dayInput 8 |> parse1 |> solve1}"

let part2 () =
    printfn $"Part 2: {dayInput 8 |> parse1 |> solve2}"
