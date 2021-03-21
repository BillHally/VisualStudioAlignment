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

let unalign (c : char) (x : string) : string =
    let lines = x.Split('\n')

    let sb = StringBuilder()

    for i in 0..(lines.Length - 1) do
        let line = lines.[i]
        let n = line.IndexOf c
        if n = -1 then
            sb.Append(line) |> ignore
        else
            let m = getLengthOfIndent line
            sb.Append(line.Substring(0, m)) |> ignore
            let mutable previousCharWasWhiteSpace = false
            for i in m..(n - 1) do
                let ch = line.[i]
                if Char.IsWhiteSpace(ch) then
                    if not previousCharWasWhiteSpace then
                        previousCharWasWhiteSpace <- true
                        sb.Append(ch) |> ignore
                else
                    previousCharWasWhiteSpace <- false
                    sb.Append(ch) |> ignore

            sb.Append(line.Substring(n)) |> ignore

        if i < lines.Length - 1 then
            sb.Append('\n') |> ignore

    sb.ToString()

let getMaxFirstIndex (c : char) (xs : string[]) : int =
    xs |> Array.fold (fun n x -> max n (x.IndexOf c)) -1

let align (c : char) (x : string) : string =
    let lines = x.Split('\n')

    if lines.Length < 2 then
        x
    else
        let maxIndex = getMaxFirstIndex c lines
        if maxIndex = -1 then
            x
        else
            let sb = StringBuilder()

            for i in 0..(lines.Length - 1) do
                let line = lines.[i]
                let n = line.IndexOf(c)
                if n = -1 then
                    sb.Append(line) |> ignore
                else
                    sb.Append(line.Substring(0, n - 1)) |> ignore

                    for _ in n..maxIndex do
                        sb.Append(' ') |> ignore

                    sb.Append(line.Substring(n)) |> ignore

                if i < lines.Length - 1 then
                    sb.Append('\n') |> ignore

            sb.ToString()

[<CompiledName("Realign")>]
let realign (c : char) (x : string) =
    x
    |> unalign c
    |> align   c

[<CompiledName("RealignAll")>]
let realignAll (x : string) =
    x
    |> unalign ':'
    |> unalign '='
    |> align   ':'
    |> align   '='
