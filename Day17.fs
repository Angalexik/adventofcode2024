module Day17

open AdventUtils
open System
open System.Text.RegularExpressions
open Utils
open Utils.Func

type State =
    {
        RegA: int
        RegB: int
        RegC: int
        PC: int
        Out: int list // IMPORTANT: THIS IS REVERSED!!! DO NOT FORGET THIS!
    }

type Combo =
    | Literal of int
    | A
    | B
    | C

type Instruction =
    | Adv of Combo
    | Bxl of int
    | Bst of Combo
    | Jnz of int
    | Bxc
    | Out of Combo
    | Bdv of Combo
    | Cdv of Combo

let combo =
    function
    | 7 -> invalidArg "arg0" "Unlucky..."
    | 6 -> C
    | 5 -> B
    | 4 -> A
    | n when n < 4 -> Literal n
    | _ -> invalidArg "arg0" "nope"

let parseInst op literal =
    match op with
    | 0 -> Adv <| combo literal
    | 1 -> Bxl literal
    | 2 -> Bst <| combo literal
    | 3 -> Jnz literal
    | 4 -> Bxc
    | 5 -> Out <| combo literal
    | 6 -> Bdv <| combo literal
    | 7 -> Cdv <| combo literal
    | _ -> invalidArg "op" "NAHH"

let step program state =
    let {
            RegA = regA
            RegB = regB
            RegC = regC
        } =
        state

    let op = Seq.item state.PC program
    let data = Seq.item (state.PC + 1) program
    let state = { state with PC = state.PC + 2 }

    let comboVal combo =
        match combo with
        | Literal n -> n
        | C -> regC
        | B -> regB
        | A -> regA

    match parseInst op data with
    | Adv combo ->
        { state with
            RegA = regA / (1 <<< comboVal combo)
        }
    | Bxl literal -> { state with RegB = regB ^^^ literal }
    | Bst combo ->
        { state with
            RegB = combo |> comboVal |> (&&&) 0b111
        }
    | Jnz literal -> if regA <> 0 then { state with PC = literal } else state
    | Bxc -> { state with RegB = regB ^^^ regC }
    | Out combo ->
        { state with
            Out = (combo |> comboVal |> (&&&) 0b111) :: state.Out
        }
    | Bdv combo ->
        { state with
            RegB = regA / (1 <<< comboVal combo)
        }
    | Cdv combo ->
        { state with
            RegC = regA / (1 <<< comboVal combo)
        }

// Don't write parsers while sleep-deprived, kids!
let parse1 input =
    let [| aLine; bLine; cLine; pLine |] =
        input
        |> Text.lines
        |> Array.filter ((<>) "")
        |> Array.map (
            (fun l -> Regex.Replace(l, @"[^\d,]", "")) >> Text.split "," >> Array.map int
        )

    pLine,
    {
        RegA = aLine[0]
        RegB = bLine[0]
        RegC = cLine[0]
        PC = 0
        Out = []
    }

let solve1 (program, initState) =
    let mutable state: State = initState
    while state.PC < Seq.length program do
        state <- step program state //|> dbg
    // state |> dbg |> _.Out |> Seq.rev |> (fun s -> String.Join(",", s))
    state.Out |> Seq.rev |> (fun s -> String.Join(",", s))

let solve2 input = ()

let test () =
    let solution = (dayTestInputs 17).[0] |> parse1 |> solve1
    printfn $"{solution}"

let part1 () =
    printfn $"Part 1: {dayInput 17 |> parse1 |> solve1}"

let part2 () =
    printfn $"Part 2: {dayInput 17 |> solve2}"
