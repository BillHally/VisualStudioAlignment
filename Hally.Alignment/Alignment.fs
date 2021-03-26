[<RequireQualifiedAccess>]
module Hally.Alignment.Alignment

open System
open System.Text

let getLengthOfIndent (x : string) : int =
    let rec loop i =
        if i = x.Length then
            i
        else if Char.IsWhiteSpace(x.[i]) then
            loop (i + 1)
        else
            i

    loop 0

[<CompiledName("Unalign")>]
let unalign (x : string) : string =
    let lines = x.Split('\n')

    let sb = StringBuilder()

    for i in 0..(lines.Length - 1) do
        let line = lines.[i]

        let m = getLengthOfIndent line
        sb.Append(line.Substring(0, m)) |> ignore

        let mutable previousCharWasWhiteSpace = false
        for ch in line.[m..^0] do
            match ch with
            | ' '
            | '\t' ->
                if not previousCharWasWhiteSpace then
                    previousCharWasWhiteSpace <- true
                    sb.Append(ch) |> ignore
            | _ ->
                previousCharWasWhiteSpace <- false
                sb.Append(ch) |> ignore

        if i < lines.Length - 1 then
            sb.Append('\n') |> ignore

    sb.ToString()

let getMaxNextIndex (startIndex : int) (s : string) (xs : string[]) : int =
    xs |> Array.fold (fun n x -> max n (if startIndex >= x.Length then -1 else x.IndexOf(s, startIndex))) -1

let rec private alignFrom (startIndex : int) (s : string) (lines : string[]) : unit =
    let maxIndex = getMaxNextIndex startIndex s lines
    if maxIndex = -1 then
        ()
    else
        let sb = StringBuilder()

        for i in 0..(lines.Length - 1) do
            let line = lines.[i]
            if startIndex < line.Length then
                let n = line.IndexOf(s, startIndex)

                // Leave the current line unchanged if it either doesn't contain s, or it contains it at the max index
                if n <> -1 && n <> maxIndex then
                    // Add the line from the start index to the the character before n
                    sb.Append(line.Substring(0, n)) |> ignore

                    // Pad the difference between the actual index for this line and the max index
                    sb.Append(' ', maxIndex - n) |> ignore

                    // Add the rest of the line
                    sb.Append(line.Substring(n)) |> ignore

                    // Update the line, and reset the builder
                    lines.[i] <- sb.ToString()
                    sb.Clear() |> ignore

        alignFrom (maxIndex + 1) s lines

let align (s : string) (x : string) : string =
    let lines = x.Split('\n')

    if lines.Length < 2 then
        x
    else
        alignFrom 0 s lines
        String.Join("\n", lines)

[<CompiledName("Realign")>]
let realign (s : string) (x : string) =
    x
    |> unalign
    |> align s

[<CompiledName("RealignAll")>]
let realignAll (x : string) : string =
    let all = [| "with member"; ":"; "="; "," |]

    x
    |> unalign
    |> (fun x -> all |> Array.fold (fun acc s -> align s acc) x)
