[<RequireQualifiedAccess>]
module Hally.Alignment.Alignment

open System

type System.Collections.Generic.List<'a> with
    member this.GetReverseIndex(rank, i) = this.Count - 1 - i

let unalignLines (lines : Line[]) : Line[] =

    Array.init lines.Length
        (fun i ->
            let tokens = lines.[i].Tokens

            let updated = ResizeArray()

            for i in 0..(tokens.Length - 1) do
                if i = 0 then
                    // Preserve the indent by keeping the first token unchanged
                    updated.Add(tokens.[0])
                else
                    let previous = updated.[^0]

                    let t = tokens.[i]

                    let t =
                        match t.Kind with
                        | Whitespace -> { t with Value = " " }
                        | _ -> t

                    let replace, start =
                        match previous.Kind, t.Kind with
                        | Whitespace, Whitespace
                        | Whitespace, Comma -> true, previous.Start
                        | _         , _ -> false, previous.Last + 1

                    if replace then updated.RemoveAt(updated.Count - 1)
                    updated.Add({ t with Start = start })

            {
                Tokens =
                    let ts = List.ofSeq updated
                    match ts with
                    | [ x    ] when x.Kind = Whitespace -> []
                    | [ x; y ] when x.Kind = Whitespace && y.Kind = Return -> [ y ]
                    | xs -> xs
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

    override this.ToString() = $"{{ Kind = {this.Kind}; MaxIndex = %2d{this.MaxIndex}; Indices = %A{this.Indices}}}"

/// When passed some lines, we want to identify which kind of token to align next - this is the one which,
/// when you take the *maximum* of the next index for that kind of token across all the lines, has the *lowest* value.
let getNextTokenKindToAlignBy shouldAlignLine (startIndex : int) (alignBy : TokenKind[]) (xs : Line[]) : AlignmentTarget option =
    let indicesByLine =
        xs
        |> Array.map (fun x -> if shouldAlignLine x then x else { Tokens = [] })
        |> getNextIndices startIndex alignBy

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

let private offsetTokens startIndex targetKind offset (tokens : Token list) =
    let rec loop tokens addOffset (acc : _ list) =
        match tokens with
        | [] -> acc
        | [t] ->
            let t =
                if addOffset then
                    { t with Start = t.Start + offset }
                else
                    t

            loop [] addOffset (t::acc)
        | t::next::xs ->
            let t, addOffset =
                if addOffset then
                    { t with Start = t.Start + offset }, addOffset
                else if next.Start >= startIndex && next.Kind = targetKind then
                    let value =
                        if t.Kind = OpenParenthesis then
                            String(' ', offset) + t.Value
                        else
                            t.Value + String(' ', offset)

                    { t with Value = value }, true
                else
                    t, addOffset

            loop (next::xs) addOffset (t::acc)

    loop tokens false [] |> List.rev

let rec private alignFrom shouldAlignLine (startIndex : int) (alignBy : TokenKind[]) (lines : Line[]) : Line[] =
    match getNextTokenKindToAlignBy shouldAlignLine startIndex alignBy lines with
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
                        let offset = a.MaxIndex - n

                        {
                            Tokens = offsetTokens startIndex a.Kind offset line.Tokens
                        }
            )

        alignFrom shouldAlignLine (a.MaxIndex + 1) alignBy updated

let private always (_ : Line) = true

let private firstNonWhitespaceToken (x : Line) : TokenKind option =
    x.Tokens |> Seq.tryPick (fun x -> match x.Kind with Whitespace -> None | k -> Some k)

let private ifFirstNonWhitespaceTokenIsSameKindAs (target : Line) =
    let targetKind = firstNonWhitespaceToken target

    fun (x : Line) -> targetKind = (firstNonWhitespaceToken x)

let alignLines (alignBy : TokenKind[]) (lines : Line[]) : Line[] =
    if lines.Length > 1 then
        alignFrom always 0 alignBy lines
    else
        lines

let alignToFirstLine (alignBy : TokenKind[]) (lines : Line[]) : Line[] =
    if lines.Length > 1 then
        alignFrom (ifFirstNonWhitespaceTokenIsSameKindAs lines.[0]) 0 alignBy lines
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
