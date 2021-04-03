namespace Hally.Alignment

type AlignmentTarget =
    | WithMember
    | Colon
    | Equals
    | Comma
    | Other

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
module AlignmentTarget =
    let all = [|
        WithMember
        Colon
        Comma
        Equals
        Other
    |]

    let getSubstring = function
        | WithMember -> Some "with member"
        | Colon      -> Some ":"
        | Equals     -> Some "="
        | Comma      -> Some ","
        | Other      -> None

    let ofString = function
        | "with member" -> WithMember
        | ":"           -> Colon
        | "="           -> Equals
        | ","           -> Comma
        | _             -> Other

    let getNextIndex (startIndex : int) (text : string) alignmentTarget : int =
        if startIndex >= text.Length then
            -1
        else
            match getSubstring alignmentTarget with
            | Some s ->
                text.IndexOf(s, startIndex)
            | None ->
                text.Substring(startIndex)
                |> Seq.tryFindIndexi
                    (
                        fun i c ->
                            let i = i + startIndex
                            if i = 0 then
                                match c with
                                | NotWhitespace -> true
                                | _             -> false
                            else
                                //printfn $"i - 1: '{text.[i-1]}' i: '{c}'"
                                match text.[i - 1], c with
                                | TabOrSpace, NotWhitespace -> true
                                | _         , _             -> false
                    )
                |> Option.map ((+) startIndex)
                |> Option.defaultValue -1
                //|> (fun x -> printfn $"Next index of (Other) after {startIndex}: %2d{x} in: '{text.TrimEnd()}'"; x)

    let getNextIndexByAlignmentTarget (startIndex : int) (text : string) (alignmentTargets : _[]) : int[] =
        alignmentTargets |> Array.map (getNextIndex startIndex text)
