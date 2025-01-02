module Day17

open AdventUtils
open System
open System.Text.RegularExpressions
open Utils
open Utils.Func

type State =
    {
        RegA: uint64
        RegB: uint64
        RegC: uint64
        PC: int
        Out: uint64 list // IMPORTANT: THIS IS REVERSED!!! DO NOT FORGET THIS!
    }

type Combo =
    | Literal of uint64
    | A
    | B
    | C

type Instruction =
    | Adv of Combo
    | Bxl of uint64
    | Bst of Combo
    | Jnz of uint64
    | Bxc
    | Out of Combo
    | Bdv of Combo
    | Cdv of Combo

let combo =
    function
    | 7UL -> invalidArg "arg0" "Unlucky..."
    | 6UL -> C
    | 5UL -> B
    | 4UL -> A
    | n when n < 4UL -> Literal n
    | _ -> invalidArg "arg0" "nope"

let parseInst op literal =
    match op with
    | 0UL -> Adv <| combo literal
    | 1UL -> Bxl literal
    | 2UL -> Bst <| combo literal
    | 3UL -> Jnz literal
    | 4UL -> Bxc
    | 5UL -> Out <| combo literal
    | 6UL -> Bdv <| combo literal
    | 7UL -> Cdv <| combo literal
    | _ -> invalidArg "op" "NAHH"

let step program state =
    let {
            RegA = regA
            RegB = regB
            RegC = regC
        } =
        state

    let op = Array.item state.PC program
    let data = Array.item (state.PC + 1) program
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
            RegA = regA >>> (comboVal combo |> int32)
        }
    | Bxl literal -> { state with RegB = regB ^^^ literal }
    | Bst combo ->
        { state with
            RegB = combo |> comboVal |> (&&&) 0b111UL
        }
    | Jnz literal ->
        if regA <> 0UL then
            { state with PC = literal |> int }
        else
            state
    | Bxc -> { state with RegB = regB ^^^ regC }
    | Out combo ->
        { state with
            Out = (combo |> comboVal |> (&&&) 0b111UL) :: state.Out
        }
    | Bdv combo ->
        { state with
            RegB = regA >>> (comboVal combo |> int32)
        }
    | Cdv combo ->
        { state with
            RegC = regA >>> (comboVal combo |> int32)
        }

// Don't write parsers while sleep-deprived, kids!
let parse1 input =
    let [| aLine; bLine; cLine; pLine |] =
        input
        |> Text.lines
        |> Array.filter ((<>) "")
        |> Array.map (
            (fun l -> Regex.Replace(l, @"[^\d,]", ""))
            >> Text.split ","
            >> Array.map uint64
        )

    pLine,
    {
        RegA = aLine[0]
        RegB = bLine[0]
        RegC = cLine[0]
        PC = 0
        Out = []
    }

let run program initState =
    let rec loop state =
        if state.PC >= Seq.length program then
            state
        else
            loop (step program state)

    loop initState |> _.Out

let solve1 (program, initState) =
    initState |> run program |> Seq.rev |> (fun s -> String.Join(",", s))

let solve2' (program, initState) =
    let pow8 x = 1UL <<< (3 * x) // Computes 8^x as 2^(3x) and 2^y = 1 << y
    // let mutable a = pow8 <| Seq.length program - 1
    // let mutable state = { initState with RegA = a }
    let run' = run program

    let expand offsets =
        ([ 0 .. (Seq.length offsets - 1) ], offsets)
        ||> Seq.zip
        |> Seq.map (fun (exp, mul) -> pow8 exp * mul)
        |> Seq.sum

    let update idx updater seq =
        seq |> Seq.indexed |> Seq.map (fun (i, x) -> if i = idx then updater x else x)

    let isProgram output =
        Array.length program = Seq.length output
        && Seq.zip program output |> Seq.map (fun (x, y) -> x = y) |> Seq.reduce (&&)

    let rec loop startingIdx curIdx offsets =
        let out = run' { initState with RegA = expand offsets } |> List.rev

        if curIdx <> startingIdx then
            printfn
                "curIdx: %A offsets: %A regA: %A output: %A"
                curIdx
                (Seq.toArray offsets)
                (expand offsets)
                out

            failwith "nah"

        if curIdx >= program.Length || curIdx >= out.Length then
            offsets |> Seq.toArray |> dbg |> ignore
            dbg curIdx |> ignore
            dbg program.Length |> ignore
            dbg out.Length |> ignore

        if isProgram out then
            expand offsets
        elif out[curIdx] = program[curIdx] then
            let newIdx = if curIdx = startingIdx then 0 else curIdx + 1
            loop startingIdx newIdx offsets
        elif Seq.item curIdx offsets >= 100UL then // probably not this one then
            loop startingIdx (curIdx + 1) (update curIdx (fun _ -> 0UL) offsets)
        else
            loop startingIdx curIdx (update curIdx ((+) 1UL) offsets)
    // loop curIdx (curAOffset + 1UL) a

    let length = Array.length program
    let offsets = Array.init length (fun i -> if i = (length - 1) then 3UL else 0UL)
    loop (length - 1) (length - 1) offsets

// Multiplying initA by 8 prepends the same value to the input every time
// Example program is some sort of weird base-8 thing
// (8^2)×3+(8^3)×5+(8^4)×4+(8^5)×3 is answer
let solve2 (program, initState) =
    let reversed = Array.rev program

    let isProgram output =
        Array.length reversed = Seq.length output
        && Seq.zip reversed output |> Seq.map (fun (x, y) -> x = y) |> Seq.reduce (&&)

    let run' = run program

    let rec loop a =
        if run' { initState with RegA = a } |> isProgram then
            a
        else
            if a &&& 0b1111111111111111UL = 0UL then // ~65k
                printfn $"{a}"

            loop (a + 1UL)

    loop 0UL

let test () =
    let solution = (dayTestInputs 17).[1] |> parse1 |> solve1
    printfn $"{solution}"

let part1 () =
    printfn $"Part 1: {dayInput 17 |> parse1 |> solve1}"

let part2 () =
    printfn $"Part 2: {dayInput 17 |> parse1 |> solve2}"
