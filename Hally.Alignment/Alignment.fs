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

let unalignLines (startIndex : int) (endIndex : int) (a : AlignmentTarget) (lines : string[]) : string[] =

    let leadingSpace = a <> Comma

    printfn $"Unaligning: {startIndex} to {endIndex}, ('{a}') preserving leading space: {leadingSpace}"

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
            match AlignmentTarget.getNextIndex charIndex line a with
            | -1        -> sb.Append(line.[charIndex]) |> ignore
            | nextIndex ->
                match line.[charIndex - 1], line.[charIndex] with
                | Eol          , TabOrSpace
                | TabOrSpace   , TabOrSpace -> ()
                | _            , Eol
                | _            , NotWhitespace -> sb.Append(line.[charIndex]) |> ignore
                | NotWhitespace, TabOrSpace ->
                    let append =
                        if leadingSpace then
                            true
                        else
                            let remaining = line.[(charIndex + 1)..(nextIndex - 1)]
                            not (String.IsNullOrWhiteSpace(remaining))

                    if append then
                        sb.Append(line.[charIndex]) |> ignore

        // Add the rest of the line
        sb.Append(line.Substring(n)) |> ignore

        updated.[lineIndex] <- sb.ToString()
        sb.Clear() |> ignore

    updated

let getNextIndices (startIndex : int) (a : AlignmentTarget) (xs : string[]) : int[] =
    xs |> Array.map (fun x -> AlignmentTarget.getNextIndex startIndex x a)

let getMaxNextIndex (startIndex : int) (a : AlignmentTarget) (xs : string[]) : int =
    xs
    |> getNextIndices startIndex a
    |> Array.max

let rec private alignFrom count (startIndex : int) (a : AlignmentTarget) (lines : string[]) : string[] =
    let nextIndices = getNextIndices startIndex a lines
    printfn $"next indices: %A{nextIndices}"
    let maxIndex = Array.max nextIndices
    if maxIndex = -1 then // || a = Other then
        lines
    else
        let sb = StringBuilder()
        let updated = Array.create lines.Length null

        for i in 0..(lines.Length - 1) do
            let line = lines.[i]

            if startIndex >= line.Length then
                // Leave the current line unchanged if it's too short to contain the startIndex
                updated.[i] <- line
            else
                let n = nextIndices.[i]
                printfn $"Aligning ({count}) from: {startIndex} ({a}): n: {n} maxIndex: {maxIndex}"

                if n = -1 || n = maxIndex then
                    // Leave the current line unchanged if it either doesn't contain s, or it contains it at the max index
                    updated.[i] <- line
                else
                    // Add the line from the start index to the the character before n
                    sb.Append(line.Substring(0, n)) |> ignore
                    printfn $"n: {n}"
                    // If appropriate, pad the difference between the actual index for this line and the max index
                    match a with
                    | Other ->
                        match line.[n - 1], line.[n] with
                        | _            , Eol
                        | NotWhitespace, NotWhitespace -> () //sb.Append(c) |> ignore
                        | Eol          , TabOrSpace
                        | Eol          , NotWhitespace
                        | TabOrSpace   , TabOrSpace
                        | TabOrSpace   , NotWhitespace
                        | NotWhitespace, _ ->
                            let diffLength = maxIndex - n
                            //printfn $"diffLength: {diffLength}"
                            if diffLength > 0 then
                                sb.Append(' ', diffLength) |> ignore
                    | _ ->
                        let diffLength = maxIndex - n
                        //printfn $"diffLength: {diffLength}"
                        if diffLength > 0 then
                            sb.Append(' ', diffLength) |> ignore

                    // Add the rest of the line
                    sb.Append(line.Substring(n)) |> ignore

                    // Update the line, and reset the builder
                    updated.[i] <- sb.ToString()
                    sb.Clear() |> ignore

        printfn $"Latest: {String.Join(string '\n', updated)}"

        if count > 10 then
            updated
        else
            alignFrom (count + 1) (maxIndex + 1) a updated

let alignLines (a : AlignmentTarget) (lines : string[]) : string[] =
    if lines.Length > 1 then
        alignFrom 0 0 a lines
    else
        lines

[<CompiledName("Unalign")>]
let unalign (startIndex : int) (endIndex : int) (s : string) (x : string) : string =
    let lines = x.Split('\n')

    let a = AlignmentTarget.ofString s

    let lines = unalignLines startIndex endIndex a lines

    String.Join("\n", lines)

[<CompiledName("Align")>]
let align (s : string) (x : string) : string =
    let lines = x.Split('\n')

    let a = AlignmentTarget.ofString s

    let lines = alignLines a lines

    String.Join("\n", lines)

[<CompiledName("Realign")>]
let realign (s : string) (x : string) =
    let lines = x.Split('\n')

    let a = AlignmentTarget.ofString s

    let lines = unalignLines 0 100 a lines
    let lines = alignLines         a lines

    String.Join("\n", lines)

[<CompiledName("UnalignAll")>]
let unalignAll (x : string) : string =
    let lines = x.Split('\n')

    let maxLength = lines |> Array.fold (fun acc x -> max acc x.Length) 0

    let lines = AlignmentTarget.all |> Array.fold (fun acc s -> let xs = unalignLines 0 maxLength s acc in printfn $"Latest: {String.Join(string '\n', xs)}"; xs) lines

    String.Join("\n", lines)

[<CompiledName("AlignAll")>]
let alignAll (x : string) : string =
    let lines = x.Split('\n')

    let lines = AlignmentTarget.all |> Array.fold (fun acc s -> let xs =   alignLines s acc in printfn $"Latest: {String.Join(string '\n', xs)}"; xs) lines

    String.Join("\n", lines)

[<CompiledName("RealignAll")>]
let realignAll (x : string) : string =
    let lines = x.Split('\n')

    let maxLength = lines |> Array.fold (fun acc x -> max acc x.Length) 0

    let lines = AlignmentTarget.all |> Array.fold (fun acc a -> unalignLines 0 maxLength a acc) lines
    let lines = AlignmentTarget.all |> Array.fold (fun acc a ->   alignLines             a acc) lines

    String.Join("\n", lines)
