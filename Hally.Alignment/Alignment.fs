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

let unalignLines (startIndex : int) (endIndex : int) (tk : TokenKind) (lines : string[]) : string[] =

    let leadingSpace = tk <> Comma

    printfn $"Unaligning: {startIndex} to {endIndex}, ('{tk}') preserving leading space: {leadingSpace}"

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
            match TokenKind.getNextIndex charIndex line tk with
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

let getNextIndices (startIndex : int) (alignBy : TokenKind[]) (xs : string[]) : int[][] =
    xs |> Array.map (fun x -> TokenKind.getNextIndexByTokenKind startIndex x alignBy)

let containsMoreThanOneNonNegative (xs : int[]) =
    let rec loop i n =
        if i >= xs.Length then
            false
        else if xs.[i] < 0 then
            loop (i + 1) n
        else if n > 0 then
            true
        else
            loop (i + 1) (n + 1)

    loop 0 0

/// When passed some lines, we want to identify which kind of token to align next - this is the one which,
/// when you take the *maximum* of the next index for that kind of token across all the lines, has the *lowest* value
let getNextTokenKindToAlignBy (startIndex : int) (alignBy : TokenKind[]) (xs : string[]) : (TokenKind * int * int[]) option =
    let indicesByLine = xs |> getNextIndices startIndex alignBy

    [|
        for i in 0..(alignBy.Length - 1) do
            let a = alignBy.[i]
            let indices = indicesByLine |> Array.map (fun x -> x.[i])
            if containsMoreThanOneNonNegative indices then
                let max = indices |> Array.max
                Some (a, max, indices)
            else
                None
    |]
    |> Array.fold (
        fun acc x ->
            match acc with
            | None -> x
            | Some (_, accMax, _) ->
                match x with
                | Some (_, m, _) when m < accMax -> x
                | _                              -> acc
        ) None

let rec private alignFrom (startIndex : int) (alignBy : TokenKind[]) (lines : string[]) already : string[] =
    match getNextTokenKindToAlignBy startIndex alignBy lines with
    | None -> lines
    | Some (a, maxIndex, nextIndices) ->
        //printfn $"next indices: {startIndex}: {a}, {maxIndex}, %A{nextIndices}"

        let sb = StringBuilder()
        let updated = Array.create lines.Length null

        for i in 0..(lines.Length - 1) do
            let line = lines.[i]

            if startIndex >= line.Length then
                // Leave the current line unchanged if it's too short to contain the startIndex
                updated.[i] <- line
            else
                let n = nextIndices.[i]
                //printfn $"Aligning from: {startIndex} ({a}): n: {n} maxIndex: {maxIndex}"

                if n = -1 || n = maxIndex then
                    // Leave the current line unchanged if it either doesn't contain s, or it contains it at the max index
                    updated.[i] <- line
                else
                    // Add the line from the start index to the the character before n
                    sb.Append(line.Substring(0, n)) |> ignore
                    //printfn $"n: {n}"
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

        //printfn $"Latest:\n{String.Join(string '\n', updated)}"

        alignFrom (maxIndex + 1) alignBy updated (already |> Set.add a)

let alignLines (alignBy : TokenKind[]) (lines : string[]) : string[] =
    if lines.Length > 1 then
        alignFrom 0 alignBy lines Set.empty
    else
        lines

[<CompiledName("Unalign")>]
let unalign (startIndex : int) (endIndex : int) (s : string) (x : string) : string =
    let lines = x.Split('\n')

    let tk = TokenKind.ofString s

    let lines = unalignLines startIndex endIndex tk lines

    String.Join("\n", lines)

[<CompiledName("Align")>]
let align (s : string) (x : string) : string =
    let lines = x.Split('\n')

    let tk = TokenKind.ofString s

    let lines = alignLines [| tk |] lines

    String.Join("\n", lines)

[<CompiledName("Realign")>]
let realign (s : string) (x : string) =
    let lines = x.Split('\n')

    let tk = TokenKind.ofString s

    let maxLength = lines |> Array.fold (fun acc x -> max acc x.Length) 0

    let lines = unalignLines 0 maxLength    tk    lines
    let lines = alignLines               [| tk |] lines

    String.Join("\n", lines)

[<CompiledName("UnalignAll")>]
let unalignAll (x : string) : string =
    let lines = x.Split('\n')

    let maxLength = lines |> Array.fold (fun acc x -> max acc x.Length) 0

    let lines =
        TokenKind.all
        |> Array.fold (
            fun acc s ->
                let xs = unalignLines 0 maxLength s acc
                printfn $"Latest: {String.Join(string '\n', xs)}"
                xs
            ) lines

    String.Join("\n", lines)

[<CompiledName("AlignAll")>]
let alignAll (x : string) : string =
    let lines = x.Split('\n')

    let lines = alignLines TokenKind.all lines //acc in printfn $"Latest: {String.Join(string '\n', xs)}"; xs) lines

    String.Join("\n", lines)

[<CompiledName("RealignAll")>]
let realignAll (x : string) : string =
    let lines = x.Split('\n')

    let maxLength = lines |> Array.fold (fun acc x -> max acc x.Length) 0

    let lines =
        TokenKind.all
        |> Array.fold (fun acc a -> unalignLines 0 maxLength a acc) lines

    let lines = alignLines (TokenKind.all (*|> Array.filter ((<>) Other)*)) lines

    String.Join("\n", lines)
