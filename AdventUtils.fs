module AdventUtils

open System
open System.IO
open System.Text.RegularExpressions

let dbg value =
    let stackTrace = Diagnostics.StackTrace(true)
    let frame = stackTrace.GetFrame(1)
    let file = frame.GetFileName()
    let line = frame.GetFileLineNumber()
    printfn "[%s:%d] Value: %A" file line value
    value

let dayInput day : string =
    File.ReadAllLines $"inputs/real/day{day.ToString()}.txt"
    |> (fun lines -> String.Join("\n", lines))

let dayTestInputs day : string array =
    let pattern = $"day{day.ToString()}" + @"_testinput(\d*)\.txt"

    Directory.GetFiles("/home/alex/Code/Advent2024/inputs")
    |> Array.choose (fun file ->
        let matsch = Regex.Match(file, pattern)

        if matsch.Success then
            Some(
                File.ReadAllLines file |> (fun lines -> String.Join("\n", lines)),
                match matsch.Groups.[1].Value with
                | "" -> -1
                | value -> value |> int
            )
        else
            None)
    |> Array.sortBy (fun (_, idx) -> idx)
    |> Array.map (fun (content, _) -> content)
