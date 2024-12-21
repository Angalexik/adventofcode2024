module Day15

open AdventUtils
open System
open Utils
open Utils.Array
open System.Text

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

type WideBoxType =
    | BoxLeft
    | BoxRight

type WideGridElement =
    | WideWall
    | WideEmpty
    | WideBox of WideBoxType

let wideGridElement =
    function
    | '#' -> WideWall
    | '@'
    | '.' -> WideEmpty
    | '[' -> WideBox BoxLeft
    | ']' -> WideBox BoxRight
    | _ -> invalidArg "_arg1" "Invalid char"

let showWideGrid robotPos grid =
    let a2d =
        grid
        |> Array2D.map (function
            | WideWall -> '#'
            | WideEmpty -> '.'
            | WideBox BoxLeft -> '['
            | WideBox BoxRight -> ']')

    a2d[fst robotPos, snd robotPos] <- '@'
    let builder = new StringBuilder()
    let height = Array2D.length1 a2d
    let width = Array2D.length2 a2d

    for y = 0 to height - 1 do
        for x = 0 to width - 1 do
            builder.Append a2d[y, x] |> ignore

        builder.AppendLine() |> ignore

    printfn $"\n\n\n{builder.ToString()}"

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

let parse2 input =
    let (grid, moves) = Text.splitOnce "\n\n" input

    let moves =
        moves
        |> _.ToCharArray()
        |> Seq.choose (fun c -> if c = '\n' then None else c |> move |> Some)
        |> Seq.toList

    let grid =
        grid
        |> Text.lines
        |> Seq.map (
            _.Replace("#", "##")
            >> _.Replace(".", "..")
            >> _.Replace("O", "[]")
            >> _.Replace("@", "@.")
        )
        |> (fun ls -> String.Join("\n", ls))
        |> charGrid

    let robotPos =
        grid
        |> Array2D.mapi (fun y x e -> ((y, x), e))
        |> flat2Darray
        |> Seq.find (fun (_, e) -> e = '@')
        |> fst

    let grid = grid |> Array2D.map wideGridElement
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

let executeMove2 (grid: WideGridElement array2d) robotPos dir =
    let rec moveBoxes y x =
        let otherHalfOffset =
            match grid[y, x] with
            | WideBox BoxLeft -> +1
            | WideBox BoxRight -> -1
            | _ -> failwithf "%A" grid

        let newY, newX = moveTo (y, x) dir
        let elementAtNewPos = grid[newY, newX]
        let otherHalfAtNewPos = grid[newY, newX + otherHalfOffset]

        let canMove =
            match dir with
            | Up
            | Down ->
                match (elementAtNewPos, otherHalfAtNewPos) with
                | (WideEmpty, WideEmpty) -> true
                | (_, WideWall)
                | (WideWall, _) -> false
                | (WideEmpty, WideBox _) -> moveBoxes newY (newX + otherHalfOffset)
                | (WideBox _, WideEmpty) -> moveBoxes newY newX
                | (a, WideBox _) when a = grid[y, x] -> moveBoxes newY newX
                | (WideBox _, WideBox _) ->
                    moveBoxes newY newX && moveBoxes newY (newX + otherHalfOffset)
            | Right
            | Left ->
                match grid[y, newX + otherHalfOffset] with
                | WideEmpty -> true
                | WideWall -> false
                | WideBox _ -> moveBoxes newY (newX + otherHalfOffset)


        // let canMove =
        //     match (grid[newY, newX], grid[newY, newX + otherHalfOffset]) with
        //     | (WideEmpty, WideEmpty) -> true
        //     | (_, WideWall)
        //     | (WideWall, _) -> false
        //     | (WideEmpty, WideBox _) -> moveBoxes newY (newX + otherHalfOffset)
        //     | (WideBox _, WideEmpty) -> moveBoxes newY newX
        //     | (WideBox _, WideBox _) ->
        //         moveBoxes newY newX || moveBoxes newY (newX + otherHalfOffset)

        if canMove then
            let firstHalf = grid[y, x]
            let secondHalf = grid[y, x + otherHalfOffset]
            grid[y, x] <- WideEmpty
            grid[y, x + otherHalfOffset] <- WideEmpty
            grid[newY, newX] <- firstHalf
            grid[newY, newX + otherHalfOffset] <- secondHalf
            true
        else
            false

    let (y, x) = moveTo robotPos dir

    match grid[y, x] with
    | WideEmpty -> (y, x)
    | WideWall -> robotPos
    | WideBox _ -> if moveBoxes y x then (y, x) else robotPos

let solve1 (grid, moves, robotPos) =
    // This feels really dirty...
    moves |> Seq.fold (fun pos dir -> executeMove grid pos dir) robotPos |> ignore

    grid
    |> Array2D.mapi (fun y x e -> if e = Box then Some(y * 100 + x) else None)
    |> flat2Darray
    |> Seq.choose id
    |> Seq.sum

let solve2 (grid, moves, robotPos) =
    // This still feels really dirty...
    moves |> Seq.fold (fun pos dir -> 
        showWideGrid pos grid
        executeMove2 grid pos dir) robotPos |> ignore

    grid
    |> Array2D.mapi (fun y x e -> if e = WideBox BoxLeft then Some(y * 100 + x) else None)
    |> flat2Darray
    |> Seq.choose id
    |> Seq.sum

let test () =
    let grid, moves, robotPos = (dayTestInputs 15).[1] |> parse2
    // let robotPos = executeMove2 grid robotPos moves.Head |> dbg
    // let moves = moves.Tail
    // let robotPos = executeMove2 grid robotPos moves.Head |> dbg
    // let moves = moves.Tail
    // let robotPos = executeMove2 grid robotPos moves.Head |> dbg
    // let moves = moves.Tail
    // let robotPos = executeMove2 grid robotPos moves.Head |> dbg
    // let moves = moves.Tail
    // let robotPos = executeMove2 grid robotPos moves.Head |> dbg
    // let moves = moves.Tail
    // let robotPos = executeMove2 grid robotPos moves.Head |> dbg
    // let moves = moves.Tail
    // let robotPos = executeMove2 grid robotPos moves.Head |> dbg
    // let moves = moves.Tail
    // let robotPos = executeMove2 grid robotPos moves.Head |> dbg
    // let moves = moves.Tail
    // let robotPos = executeMove2 grid robotPos moves.Head |> dbg
    // let moves = moves.Tail
    // let robotPos = executeMove2 grid robotPos moves.Head |> dbg
    // let moves = moves.Tail
    // let robotPos = executeMove2 grid robotPos moves.Head |> dbg
    // let moves = moves.Tail
    dbg grid |> ignore
    let solution = (grid, moves, robotPos) |> solve2
    printfn $"{solution}"

let part1 () =
    printfn $"Part 1: {dayInput 15 |> parse1 |> solve1}"

let part2 () =
    printfn $"Part 2: {dayInput 15 |> parse2 |> solve2}"
