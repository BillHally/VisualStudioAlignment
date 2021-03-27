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

let (|Whitespace|Other|) x =
    match x with
    | ' ' | '\t' -> Whitespace x
    | _          -> Other      x

let getNextIndex (startIndex : int) (s : string) (x : string) : int =
    if startIndex >= x.Length then -1 else x.IndexOf(s, startIndex)

let unalignLines (startIndex : int) (endIndex : int) (s : string) (lines : string[]) : unit =

    let leadingSpace = s <> ","

    printfn $"Unaligning: {startIndex} to {endIndex}, preserving leading space: {leadingSpace}"

    let sb = StringBuilder()

    for lineIndex in 0..(lines.Length - 1) do
        let line = lines.[lineIndex]

        let m = min line.Length (max startIndex (getLengthOfIndent line))

        // Add the line from the beginning to the max of start index and the end of the indent
        sb.Append(line.Substring(0, m)) |> ignore

        let n = min line.Length endIndex

        // Unalign
        for charIndex in m..(n - 1) do
            match line.[charIndex - 1], line.[charIndex] with
            | Whitespace _, Whitespace _ -> ()
            | Whitespace _, Other      c
            | Other      _, Other      c -> sb.Append(c) |> ignore
            | Other      _, Whitespace c ->
                let append =
                    if leadingSpace then
                        true
                    else
                        let nextIndex =
                            match getNextIndex charIndex s line with
                            | -1        -> n
                            | nextIndex -> nextIndex

                        let remaining = line.[(charIndex + 1)..(nextIndex - 1)]
                        not (String.IsNullOrWhiteSpace(remaining))

                if append then
                    sb.Append(c) |> ignore

        // Add the rest of the line
        sb.Append(line.Substring(n)) |> ignore

        lines.[lineIndex] <- sb.ToString()
        sb.Clear() |> ignore

let getMaxNextIndex (startIndex : int) (s : string) (xs : string[]) : int =
    xs |> Array.fold (fun n x -> max n (getNextIndex startIndex s x)) -1

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

let alignLines (s : string) (lines : string[]) : unit =
    if lines.Length > 1 then
        alignFrom 0 s lines

[<CompiledName("Unalign")>]
let unalign (startIndex : int) (endIndex : int) (s : string) (x : string) : string =
    let lines = x.Split('\n')
    unalignLines startIndex endIndex s lines
    String.Join("\n", lines)

[<CompiledName("Align")>]
let align (s : string) (x : string) : string =
    let lines = x.Split('\n')
    alignLines s lines
    String.Join("\n", lines)

[<CompiledName("Realign")>]
let realign (s : string) (x : string) =
    let lines = x.Split('\n')

    unalignLines 0 100 s lines
    alignLines         s lines

    String.Join("\n", lines)

[<CompiledName("RealignAll")>]
let realignAll (x : string) : string =
    let all = [| "with member"; ":"; "="; "," |]

    let lines = x.Split('\n')

    let maxLength = lines |> Array.fold (fun acc x -> max acc x.Length) 0

    all |> Array.fold (fun () s -> unalignLines 0 maxLength s lines) ()
    all |> Array.fold (fun () s ->   alignLines             s lines) ()

    String.Join("\n", lines)
