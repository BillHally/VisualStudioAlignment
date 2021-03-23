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

let getMaxFirstIndex (s : string) (xs : string[]) : int =
    xs |> Array.fold (fun n x -> max n (x.IndexOf(s))) -1

let align (s : string) (x : string) : string =
    let lines = x.Split('\n')

    if lines.Length < 2 then
        x
    else
        let maxIndex = getMaxFirstIndex s lines
        if maxIndex = -1 then
            x
        else
            let sb = StringBuilder()

            for i in 0..(lines.Length - 1) do
                let line = lines.[i]
                let n = line.IndexOf(s)
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
let realign (s : string) (x : string) =
    x
    |> unalign
    |> align s

[<CompiledName("RealignAll")>]
let realignAll (x : string) : string =
    let all = [| "with member"; ":"; "=" |]

    x
    |> unalign
    |> (fun x -> all |> Array.fold (fun acc s -> align s acc) x)
