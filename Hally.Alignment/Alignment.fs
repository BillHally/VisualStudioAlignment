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
    if startIndex >= x.Length then
        -1
    else
        match s with
        | "" ->
            x.Substring(startIndex)
            |> Seq.tryFindIndex
                (
                    fun c ->
                        match c with
                        | Whitespace _ -> true
                        |            _ -> false
                )
            |> Option.map ((+) startIndex)
            |> Option.defaultValue -1
        | s ->
            x.IndexOf(s, startIndex)

let unalignLines (startIndex : int) (endIndex : int) (s : string) (lines : string[]) : string[] =

    let leadingSpace = s <> ","

    printfn $"Unaligning: {startIndex} to {endIndex}, ('{s}') preserving leading space: {leadingSpace}"

    let sb = StringBuilder()

    let updated = Array.copy lines

    for lineIndex in 0..(updated.Length - 1) do
        let line = updated.[lineIndex]

        let m = min line.Length (max startIndex (getLengthOfIndent line))

        // Add the line from the beginning to the max of start index and the end of the indent
        sb.Append(line.Substring(0, m)) |> ignore

        let n = min line.Length endIndex

        // Unalign
        for charIndex in m..(n - 1) do
            match getNextIndex charIndex s line with
            | -1        -> sb.Append(line.[charIndex]) |> ignore
            | nextIndex ->
                match line.[charIndex - 1], line.[charIndex] with
                | Whitespace _, Whitespace _ -> ()
                | Whitespace _, Other      c
                | Other      _, Other      c -> sb.Append(c) |> ignore
                | Other      _, Whitespace c ->
                    let append =
                        if leadingSpace then
                            true
                        else
                            let remaining = line.[(charIndex + 1)..(nextIndex - 1)]
                            not (String.IsNullOrWhiteSpace(remaining))

                    if append then
                        sb.Append(c) |> ignore

        // Add the rest of the line
        sb.Append(line.Substring(n)) |> ignore

        updated.[lineIndex] <- sb.ToString()
        sb.Clear() |> ignore

    updated

let getMaxNextIndex (startIndex : int) (s : string) (xs : string[]) : int =
    let result = xs |> Array.fold (fun n x -> max n (getNextIndex startIndex s x)) -1
    printfn $"startIndex: {startIndex} result: {result}"
    result

let rec private alignFrom count (startIndex : int) (s : string) (lines : string[]) : string[] =
    printfn $"Aligning ({count}) from: {startIndex} ('{s}')"
    let maxIndex = getMaxNextIndex startIndex s lines
    if maxIndex = -1 || s = "" then // TODO: remove the need for this empty string comparison
        lines
    else
        let sb = StringBuilder()
        let updated = Array.create lines.Length null
        for i in 0..(lines.Length - 1) do
            let line = lines.[i]
            if startIndex < line.Length then
                let n = line.IndexOf(s, startIndex)

                // Leave the current line unchanged if it either doesn't contain s, or it contains it at the max index
                if n <> -1 && n <> maxIndex then
                    // Add the line from the start index to the the character before n
                    sb.Append(line.Substring(0, n)) |> ignore

                    // Pad the difference between the actual index for this line and the max index
                    printfn $"maxIndex - n: {maxIndex - n}"
                    let diffLength = maxIndex - n
                    if diffLength > 0 then
                        sb.Append(' ', diffLength) |> ignore

                    // Add the rest of the line
                    sb.Append(line.Substring(n)) |> ignore

                    // Update the line, and reset the builder
                    updated.[i] <- sb.ToString()
                    sb.Clear() |> ignore
                else
                    updated.[i] <- line
            else
                updated.[i] <- line

        if count > 10 then
            updated
        else
            alignFrom (count + 1) (maxIndex + 1) s updated

let alignLines (s : string) (lines : string[]) : string[] =
    if lines.Length > 1 then
        alignFrom 0 0 s lines
    else
        lines

let private all = [| "with member"; ":"; "="; "," |]

[<CompiledName("Unalign")>]
let unalign (startIndex : int) (endIndex : int) (s : string) (x : string) : string =
    let lines = x.Split('\n')

    let lines = unalignLines startIndex endIndex s lines

    String.Join("\n", lines)

[<CompiledName("Align")>]
let align (s : string) (x : string) : string =
    let lines = x.Split('\n')

    let lines = alignLines s lines

    String.Join("\n", lines)

[<CompiledName("Realign")>]
let realign (s : string) (x : string) =
    let lines = x.Split('\n')

    let lines = unalignLines 0 100 s lines
    let lines = alignLines         s lines

    String.Join("\n", lines)

[<CompiledName("UnalignAll")>]
let unalignAll (x : string) : string =
    let lines = x.Split('\n')

    let maxLength = lines |> Array.fold (fun acc x -> max acc x.Length) 0

    let lines = all |> Array.fold (fun acc s -> let xs = unalignLines 0 maxLength s acc in printfn $"Latest: {String.Join(string '\n', xs)}"; xs) lines

    String.Join("\n", lines)

[<CompiledName("AlignAll")>]
let alignAll (x : string) : string =
    let lines = x.Split('\n')

    let lines = all |> Array.fold (fun acc s -> let xs =   alignLines s acc in printfn $"Latest: {String.Join(string '\n', xs)}"; xs) lines

    String.Join("\n", lines)

[<CompiledName("RealignAll")>]
let realignAll (x : string) : string =
    let lines = x.Split('\n')

    let maxLength = lines |> Array.fold (fun acc x -> max acc x.Length) 0

    let lines = all |> Array.fold (fun acc s -> let xs = unalignLines 0 maxLength s acc in printfn $"Latest: {String.Join(string '\n', xs)}"; xs) lines
    let lines = all |> Array.fold (fun acc s -> let xs =   alignLines             s acc in printfn $"Latest: {String.Join(string '\n', xs)}"; xs) lines

    String.Join("\n", lines)
