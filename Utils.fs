module Utils

open System.Text.RegularExpressions

module Text =
    let split (separator: string) (text: string) = text.Split(separator)

    let splitOnce (separator: string) (text: string) =
        let pos = text.IndexOf(separator)

        if pos < 0 then
            invalidArg "text" "Separator not found in string"
        else
            (text.Substring(0, pos), text.Substring(pos + separator.Length))

    let indexesOf (subString: string) (text: string) =
        Regex.Matches(text, Regex.Escape(subString)) |> Seq.map _.Index

    let chars (text: string) = text.ToCharArray()

    let charToInt (c: char) = int c - int '0'
