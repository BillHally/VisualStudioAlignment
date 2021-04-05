[<RequireQualifiedAccess>]
module Hally.Alignment.Alignment

open System

let unalignLines (tk : TokenKind) (lines : Line[]) : Line[] =

    Array.init lines.Length
        (fun i ->
            let tokens = lines.[i].Tokens

            let updated = ResizeArray()

            for i in 0..(tokens.Length - 1) do
                if i = 0 then
                    tokens.[0]
                else
                    let previous = updated.[i - 1]

                    let t = tokens.[i]

                    let start =
                        match t.Kind with
                        | Comma
                        | Return -> previous.Last + 1
                        | _      -> previous.Last + 2
                    {
                        t with
                            Start = start
                            Last  = t.Value.Length + start - 1
                    }
                |> updated.Add

            {
                Tokens = List.ofSeq updated
            }
        )

let getNextIndices (startIndex : int) (alignBy : TokenKind[]) (xs : Line[]) : int[][] =
    xs |> Array.map (fun x -> Line.getNextIndexByTokenKind startIndex x alignBy)

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

type AlignmentTarget =
    {
        Kind     : TokenKind
        Indices  : int[]
        MaxIndex : int
    }

/// When passed some lines, we want to identify which kind of token to align next - this is the one which,
/// when you take the *maximum* of the next index for that kind of token across all the lines, has the *lowest* value.
let getNextTokenKindToAlignBy (startIndex : int) (alignBy : TokenKind[]) (xs : Line[]) : AlignmentTarget option =
    let indicesByLine = xs |> getNextIndices startIndex alignBy

    [|
        for i in 0..(alignBy.Length - 1) do
            let a = alignBy.[i]
            let indices = indicesByLine |> Array.map (fun x -> x.[i])
            if containsMoreThanOneNonNegative indices then
                let max = indices |> Array.max
                Some
                    {
                        Kind    = a
                        Indices = indices
                        MaxIndex = max
                    }
            else
                None
    |]
    |> Array.fold (
        fun acc x ->
            match acc with
            | None -> x
            | Some acc ->
                match x with
                | Some x when x.MaxIndex < acc.MaxIndex -> Some x
                | _ -> Some acc
        ) None

let rec private alignFrom (startIndex : int) (alignBy : TokenKind[]) (lines : Line[]) already : Line[] =
    match getNextTokenKindToAlignBy startIndex alignBy lines with
    | None -> lines
    | Some a ->
        let updated =
            Array.init lines.Length
                (fun i ->
                    let line = lines.[i]

                    if startIndex >= line.Length then
                        // Leave the current line unchanged if it's too short to contain the startIndex
                        line
                    else
                        let n = a.Indices.[i]

                        if n = -1 || n = a.MaxIndex then
                            // Leave the current line unchanged if it either doesn't contain s, or it contains it at the max index
                            line
                        else
                            // If appropriate, pad the difference between the actual index for this line and the max index
                            let padding = a.MaxIndex - n

                            let mutable addPadding = false

                            {
                                Tokens =
                                    List.init line.Tokens.Length
                                        (fun i ->
                                            let t = line.Tokens.[i]
                                            if addPadding || t.Start > startIndex && t.Kind = a.Kind then
                                                addPadding <- true
                                                {
                                                    t with
                                                        Start = t.Start + padding
                                                        Last  = t.Last  + padding
                                                }
                                            else
                                                t
                                        )
                            }
                )

        alignFrom (a.MaxIndex + 1) alignBy updated (already |> Set.add a.Kind)

let alignLines (alignBy : TokenKind[]) (lines : Line[]) : Line[] =
    if lines.Length > 1 then
        alignFrom 0 alignBy lines Set.empty
    else
        lines

[<CompiledName("Unalign")>]
let unalign (s : string) (x : string) : string =
    let lines = x.Split('\n') |> Array.map Line.ofString

    let tk = TokenKind.ofString s

    let lines = unalignLines tk lines

    let lines = lines |> Array.map Line.toString
    String.Join("\n", lines)

[<CompiledName("Align")>]
let align (s : string) (x : string) : string =
    let lines = x.Split('\n') |> Array.map Line.ofString

    let tk = TokenKind.ofString s

    let lines = alignLines [| tk |] lines

    let lines = lines |> Array.map Line.toString
    String.Join("\n", lines)

[<CompiledName("Realign")>]
let realign (s : string) (x : string) =
    let lines = x.Split('\n') |> Array.map Line.ofString

    let tk = TokenKind.ofString s

    let lines = unalignLines    tk    lines
    let lines = alignLines   [| tk |] lines

    let lines = lines |> Array.map Line.toString
    String.Join("\n", lines)

[<CompiledName("UnalignAll")>]
let unalignAll (x : string) : string =
    let lines = x.Split('\n') |> Array.map Line.ofString

    let lines =
        TokenKind.all
        |> Array.fold (fun acc s -> unalignLines s acc) lines

    let lines = lines |> Array.map Line.toString
    String.Join("\n", lines)

[<CompiledName("AlignAll")>]
let alignAll (x : string) : string =
    let lines = x.Split('\n') |> Array.map Line.ofString

    let lines = alignLines TokenKind.all lines

    let lines = lines |> Array.map Line.toString
    String.Join("\n", lines)

[<CompiledName("RealignAll")>]
let realignAll (x : string) : string =
    let lines = x.Split('\n') |> Array.map Line.ofString

    let lines =
        TokenKind.all
        |> Array.fold (fun acc a -> unalignLines a acc) lines

    let lines = alignLines TokenKind.all lines

    let lines = lines |> Array.map Line.toString
    String.Join("\n", lines)
