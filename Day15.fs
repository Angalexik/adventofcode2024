module Day15

open AdventUtils
open System
open Utils
open Utils.Array

let (++) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

type Move =
    | Up
    | Down
    | Left
    | Right

let move =
    function
    | '^' -> Up
    | 'v' -> Down
    | '>' -> Right
    | '<' -> Left
    | _ -> invalidArg "_arg1" "Invalid char"

let moveTo start dir =
    let add = (++) start

    match dir with
    | Up -> add (-1, 0)
    | Down -> add (1, 0)
    | Right -> add (0, 1)
    | Left -> add (0, -1)

type GridElement =
    | Wall
    | Box
    | Empty

let gridElement =
    function
    | '#' -> Wall
    | 'O' -> Box
    | '.'
    | '@' -> Empty
    | _ -> invalidArg "_arg1" "Invalid char"

let parse1 input =
    let (grid, moves) = Text.splitOnce "\n\n" input

    let moves =
        moves
        |> _.ToCharArray()
        |> Seq.choose (fun c -> if c = '\n' then None else c |> move |> Some)
        |> Seq.toList

    let grid = grid |> charGrid

    let robotPos =
        grid
        |> Array2D.mapi (fun y x e -> ((y, x), e))
        |> flat2Darray
        |> Seq.find (fun (_, e) -> e = '@')
        |> fst

    let grid = grid |> Array2D.map gridElement
    (grid, moves, robotPos)

let executeMove (grid: GridElement array2d) robotPos dir =
    let rec moveBoxes y x =
        let newY, newX = moveTo (y, x) dir

        let canMove =
            match grid[newY, newX] with
            | Empty -> true
            | Wall -> false
            | Box -> moveBoxes newY newX

        if canMove then
            grid[y, x] <- Empty
            grid[newY, newX] <- Box
            true
        else
            false

    let (y, x) = moveTo robotPos dir

    match grid[y, x] with
    | Empty -> (y, x)
    | Wall -> robotPos
    | Box -> if moveBoxes y x then (y, x) else robotPos

let solve1 (grid, moves, robotPos) =
    // This feels really dirty...
    moves |> Seq.fold (fun pos dir -> executeMove grid pos dir) robotPos |> ignore

    grid
    |> Array2D.mapi (fun y x e -> if e = Box then Some(y * 100 + x) else None)
    |> flat2Darray
    |> Seq.choose id
    |> Seq.sum

let solve2 input = ()

let test () =
    let grid, moves, robotPos = (dayTestInputs 15).[1] |> parse1
    // let robotPos = executeMove grid robotPos moves.Head |> dbg
    // let moves = moves.Tail
    // let robotPos = executeMove grid robotPos moves.Head |> dbg
    // let moves = moves.Tail
    // let robotPos = executeMove grid robotPos moves.Head |> dbg
    // let moves = moves.Tail
    // let robotPos = executeMove grid robotPos moves.Head |> dbg
    // dbg grid |> ignore
    let solution = (grid, moves, robotPos) |> solve1
    printfn $"{solution}"

let part1 () =
    printfn $"Part 1: {dayInput 15 |> parse1 |> solve1}"

let part2 () =
    printfn $"Part 2: {dayInput 15 |> solve2}"
