[<RequireQualifiedAccess>]
module Hally.Alignment.Alignment

open System

let unalignLines (lines : Line[]) : Line[] =

    Array.init lines.Length
        (fun i ->
            let tokens = lines.[i].Tokens |> List.filter (fun x -> x.Kind <> Whitespace)

            let updated = ResizeArray()

            for i in 0..(tokens.Length - 1) do
                if i = 0 then
                    tokens.[0]
                else
                    let previous = updated.[i - 1]

                    let t = tokens.[i]

                    let start =
                        match previous.Kind, t.Kind with
                        | _         , Comma
                        | _         , SemiColon
                        // | Colon // TODO: support no leading space for Colon?. What about other token kinds?
                        | _         , Whitespace
                        | _         , Return
                        | Whitespace, _
                        | Return    , _ -> previous.Last + 1
                        | _         , _ -> previous.Last + 2

                    {
                        t with
                            Start = start
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
                        Kind     = a
                        Indices  = indices
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

let rec private alignFrom shouldAlignLine (startIndex : int) (alignBy : TokenKind[]) (lines : Line[]) already : Line[] =
    let tokenLines = lines |> Array.map (fun x -> if shouldAlignLine x then x else { Tokens = [] }) // TODO: Improve this
    match getNextTokenKindToAlignBy startIndex alignBy tokenLines with
    | None -> lines
    | Some a ->
        let updated =
            Array.init lines.Length (fun i ->
                let line = lines.[i]

                if startIndex >= line.Length || not (shouldAlignLine line) then
                    // Leave the current line unchanged if it's too short, or shouldn't be aligned
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
                                List.init line.Tokens.Length (fun i ->
                                    let t = line.Tokens.[i]

                                    if addPadding || t.Start >= startIndex && t.Kind = a.Kind then
                                        addPadding <- true
                                        {
                                            t with
                                                Start = t.Start + padding
                                        }
                                    else
                                        t
                                )
                        }
            )

        alignFrom shouldAlignLine (a.MaxIndex + 1) alignBy updated (already |> Set.add a.Kind)

let private always (_ : Line) = true

let private ifFirstTokenIsSameKindAs (target : Line) (x : Line) : bool =
    match target.Tokens, x.Tokens with
    | x::_, y::_ -> x.Kind = y.Kind
    | _   , _    -> false

let alignLines (alignBy : TokenKind[]) (lines : Line[]) : Line[] =
    if lines.Length > 1 then
        alignFrom always 0 alignBy lines Set.empty
    else
        lines

let alignToFirstLine (alignBy : TokenKind[]) (lines : Line[]) : Line[] =
    if lines.Length > 1 then
        alignFrom (ifFirstTokenIsSameKindAs lines.[0]) 0 alignBy lines Set.empty
    else
        lines

[<CompiledName("Unalign")>]
let unalign (x : string) : string =
    x.Split('\n')
    |> Array.map Line.ofString
    |> unalignLines
    |> Array.map Line.toString
    |> String.concat "\n"

[<CompiledName("Align")>]
let align (s : string) (x : string) : string =
    let lines = x.Split('\n') |> Array.map Line.ofString

    let tk = TokenKind.ofString s

    let lines = alignLines [| tk |] lines

    let lines = lines |> Array.map Line.toString
    String.Join("\n", lines)

[<CompiledName("UnalignAll")>]
let unalignAll (xs : string[]) : string[] =
    xs
    |> Array.map Line.ofString
    |> unalignLines
    |> Array.map Line.toString

[<CompiledName("AlignAll")>]
let alignAll (xs : string[]) : string[] =
    xs
    |> Array.map Line.ofString
    |> alignLines TokenKind.all
    |> Array.map Line.toString

[<CompiledName("AlignAllExtended")>]
let alignAllExtended (xs : string[]) : string[] =
    xs
    |> Array.map Line.ofString
    |> alignLines TokenKind.allExtended
    |> Array.map Line.toString

[<CompiledName("RealignAll")>]
let realignAll (xs : string[]) : string[] =
    xs
    |> Array.map Line.ofString
    |> unalignLines
    |> alignLines TokenKind.all
    |> Array.map Line.toString

[<CompiledName("RealignToFirstLine")>]
let realignToFirstLine (xs : string[]) : string[] =
    xs
    |> Array.map Line.ofString
    |> unalignLines
    |> alignToFirstLine TokenKind.all
    |> Array.map Line.toString

[<CompiledName("RealignToFirstLineExtended")>]
let realignToFirstLineExtended (xs : string[]) : string[] =
    xs
    |> Array.map Line.ofString
    |> unalignLines
    |> alignToFirstLine TokenKind.allExtended
    |> Array.map Line.toString

[<CompiledName("RealignAllExtended")>]
let realignAllExtended (xs : string[]) : string[] =
    xs
    |> Array.map Line.ofString
    |> unalignLines
    |> alignLines TokenKind.allExtended
    |> Array.map Line.toString
