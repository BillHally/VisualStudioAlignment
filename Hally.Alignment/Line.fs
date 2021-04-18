namespace Hally.Alignment

open System
open System.Text

type Line =
    {
        Tokens : Token list
    }

    member this.Length =
        match this.Tokens |> List.tryLast with
        | Some x -> x.Last + 1
        | None   -> 0

    override this.ToString() = this.Tokens |> Seq.map (fun x -> x.ToString()) |> String.concat Environment.NewLine

[<RequireQualifiedAccess>]
module Line =
    let private (| TabOrSpace | Eol | NotWhitespace |) = function
        | ' '
        | '\t' -> TabOrSpace
        | '\r' -> Eol
        | '\n' -> Eol // Shouldn't ever happen - we split on newlines
        | _    -> NotWhitespace

    let private basicOfString (text : string) : Line =
        let tokens = ResizeArray<Token>()
        let mutable start = None

        for i in 0..(text.Length - 1) do
            let c = text.[i]
            let current =
                match c with
                | TabOrSpace ->
                    match start with
                    | None -> None
                    | Some x ->
                        start <- None
                        Some (x, (i - 1))
                | Eol ->
                    match start with
                    | None ->
                        start <- Some i
                        None
                    | Some x ->
                        start <- Some i
                        Some (x, (i - 1))
                | NotWhitespace ->
                    match start with
                    | None ->
                        start <- Some i
                        None
                    | Some x ->
                        // Currently, we only permit Comma and SemiColon to have no leading space, so just check for those
                        match c with
                        | ','
                        | ';' ->
                            start <- Some i
                            Some (x, (i - 1))
                        |_ ->
                            None

            match current with
            | Some (start, last) ->
                let v = text.[start..last]
                {
                    Kind  = TokenKind.ofString v
                    Value = v
                    Start = start
                    Last  = last
                }
                |> tokens.Add
            | None -> ()

        // Check if we're still in a token, and add it if so (should only happen for Eol)
        match start with
        | Some start ->
            let last = text.Length - 1
            let v = text.[start..last]
            {
                Kind  = TokenKind.ofString v
                Value = v
                Start = start
                Last  = last
            }
            |> tokens.Add
        | None -> ()

        // Don't lose trailing whitespace (TODO: make this configurable)
        match tokens |> Seq.tryLast with
        | Some x ->
            if x.Last <> text.Length - 1 then
                let start = x.Last + 1
                let last = text.Length - 1
                let v = text.[start..last]
                {
                    Kind  = TokenKind.ofString v
                    Value = v
                    Start = start
                    Last  = last
                }
                |> tokens.Add
        | None ->
            let start = 0
            let last = text.Length - 1
            let v = text.[start..last]
            {
                Kind  = TokenKind.ofString v
                Value = v
                Start = start
                Last  = last
            }
            |> tokens.Add

        {
            Tokens = List.ofSeq tokens
        }

    let ofString (text : string) : Line =
        if text.Length = 0 then
            {
                Tokens = []
            }
        else
            match Parsing.tryTokenize text with
            | Ok xs ->
                {
                    Tokens = xs
                }
            | Error (s, e) ->
                // TODO: when running under debug, throw an exception holding the error messages
                // Fall back to some basic string parsing (Will align *inside* string literals etc., but OK most of the time)
                basicOfString text

    /// Gets the next index for the given token kind.
    ///
    /// However, if aligning either Comma or Other, we don't want to align to tokens on different sides of a token
    /// which is neither - so only consider tokens which have not been preceded by something else.
    let getNextIndex (startIndex : int) (line : Line) tk : int =
        let rec commaOrOtherLoop xs =
            match xs with
            | [] -> -1
            | x::xs ->
                if x.Start < startIndex then
                    commaOrOtherLoop xs
                else
                    match x.Kind with
                    | Comma
                    | Other
                    | Whitespace
                    | Return ->
                        if x.Kind = tk then
                            x.Start
                        else
                            commaOrOtherLoop xs
                    | _ -> -1

        match tk with
        | Comma
        | Other -> commaOrOtherLoop line.Tokens
        | _ ->
            line.Tokens
            |> List.tryFind (fun t -> t.Kind = tk && t.Start >= startIndex)
            |> function
                | Some t -> t.Start
                | None   -> -1

    let getNextIndexByTokenKind (startIndex : int) (line : Line) (xs : TokenKind[]) : int[] =
        xs |> Array.map (getNextIndex startIndex line)

    let toString (x : Line) : string =
        let sb = StringBuilder(x.Length)
        let mutable current = 0

        for t in x.Tokens do
            // If necessary, add padding
            match t.Kind with
            | Whitespace
            | Return -> ()
            | _ ->
                sb.Append(String(' ', t.Start - current)) |> ignore

            // Add the token
            sb.Append(t.Value) |> ignore

            // Record where we're up to
            current <- t.Last + 1

        sb.ToString()
