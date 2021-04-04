namespace Hally.Alignment

open System

type TokenKind =
    | With
    | Member
    | Colon
    | Equals
    | Comma
    | Type
    | OpenBrace
    | CloseBrace
    | Other

type Token =
    {
        Kind  : TokenKind
        Value : string
        Start : int
    }

    override this.ToString() = $"%2d{this.Start}:{this.Kind}: \"{this.Value}\""

type Line =
    {
        Tokens : Token[]
    }

    override this.ToString() = this.Tokens |> Seq.map (fun x -> x.ToString()) |> String.concat Environment.NewLine

[<AutoOpen>]
module CharActivePatterns =
    let (|TabOrSpace|Eol|NotWhitespace|) x =
        match x with
        | ' '
        | '\t' -> TabOrSpace
        | '\r'
        | '\n' -> Eol
        | _    -> NotWhitespace

[<RequireQualifiedAccess>]
module TokenKind =
    let all = [|
        With
        Member
        Colon
        Comma
        Equals
        //Type
        //OpenBrace
        //CloseBrace
        Other
    |]

    //let getSubstring = function
    //    | With   -> Some "member"
    //    | Member -> Some "member"
    //    | Colon  -> Some ":"
    //    | Equals -> Some "="
    //    | Comma  -> Some ","
    //    | Other  -> None

    let ofString = function
        | "with"   -> With
        | "member" -> Member
        | ":"      -> Colon
        | "="      -> Equals
        | ","      -> Comma
        | "{"      -> OpenBrace
        | "}"      -> CloseBrace
        | "type"   -> Type
        | _        -> Other

    let tokenize (text : string) : Line =
        let tokens = ResizeArray<Token>()
        let mutable start = None

        for i in 0..(text.Length - 1) do
            let c = text.[i]
            let current =

                match c with
                | TabOrSpace
                | Eol ->
                    match start with
                    | None -> None
                    | Some x ->
                        start <- None
                        Some (x, (i - 1))
                | NotWhitespace ->
                    match start with
                    | None ->
                        start <- Some i
                        None
                    | Some x ->
                        // Currently, we only permit Comma to have no leading space, so just check for that
                        if c = ',' then
                            start <- Some i
                            Some (x, (i - 1))
                        else
                            None

            match current with
            | Some (start, last) ->
                let v = text.[start..last]
                {
                    Kind  = ofString v
                    Value = v
                    Start = start
                }
                |> tokens.Add
            | None -> ()

        { Tokens = Array.ofSeq tokens }

    //let getNextIndex' (startIndex : int) (text : string) tk : int =
    //    if startIndex >= text.Length then
    //        -1
    //    else
    //        match getSubstring tk with
    //        | Some s ->
    //            text.IndexOf(s, startIndex)
    //        | None ->
    //            text.Substring(startIndex)
    //            |> Seq.tryFindIndexi
    //                (
    //                    fun i c ->
    //                        let i = i + startIndex
    //                        if i = 0 then
    //                            match c with
    //                            | NotWhitespace -> true
    //                            | _             -> false
    //                        else
    //                            //printfn $"i - 1: '{text.[i-1]}' i: '{c}'"
    //                            match text.[i - 1], c with
    //                            | TabOrSpace, NotWhitespace -> true
    //                            | _         , _             -> false
    //                )
    //            |> Option.map ((+) startIndex)
    //            |> Option.defaultValue -1
    //            //|> (fun x -> printfn $"Next index of (Other) after {startIndex}: %2d{x} in: '{text.TrimEnd()}'"; x)

    let getNextIndex'' (startIndex : int) (x : Line) tk : int =
        x.Tokens
        |> Seq.tryFind (fun t -> t.Kind = tk && t.Start >= startIndex)
        |> function
            | Some t -> t.Start
            | None   -> -1

    let getNextIndex (startIndex : int) (text : string) tk : int =
        printfn " 0123456789012345678901234567890"
        printfn $"\"{text}\""

        text
        |> tokenize
        |> (fun x -> printfn $"{x}"; x.Tokens)
        |> Seq.tryFind (fun t -> t.Kind = tk && t.Start >= startIndex)
        |> function
            | Some t -> t.Start
            | None   -> -1

    let getNextIndexByTokenKind (startIndex : int) (text : string) (xs : TokenKind[]) : int[] =
        xs |> Array.map (getNextIndex startIndex text)
