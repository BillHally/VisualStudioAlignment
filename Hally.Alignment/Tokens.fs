namespace Hally.Alignment

type TokenKind =
    | With
    | Member
    | Colon
    | SemiColon
    | Equals
    | Comma
    | Type
    | OpenBrace
    | CloseBrace
    | ForwardPipe
    | BackwardPipe
    | ForwardArrow
    | BackwardArrow
    | LiteralString
    // TODO:
    // | Or
    // | And
    // | [
    // | ]
    // | (
    // | )
    // | .
    // Operators
    | XmlCloseTag
    | Return
    | Whitespace
    | Other

type Token =
    {
        Kind  : TokenKind
        Value : string
        Start : int
    }

    member this.Last = this.Start + this.Value.Length - 1

    override this.ToString() =
        let value = $"\"{match this.Kind with Return -> string '⏎' | _ -> this.Value}\""
        $"%03d{this.Start}-%03d{this.Last}:%13O{this.Kind}: %s{value}"

[<RequireQualifiedAccess>]
module TokenKind =
    let allExtended = [|
        With
        Member
        Colon
        SemiColon
        Comma
        Equals
        Type
        OpenBrace
        CloseBrace
        ForwardPipe
        BackwardPipe
        ForwardArrow
        BackwardArrow
        LiteralString
        XmlCloseTag
        Other
    |]

    let all = [|
        With
        Member
        Colon
        SemiColon
        Comma
        Equals
        OpenBrace
        CloseBrace
        ForwardPipe
        BackwardPipe
        ForwardArrow
        BackwardArrow
        LiteralString
        XmlCloseTag
    |]

    let ofString = function
        | "with"   -> With
        | "member" -> Member
        | ":"      -> Colon
        | ";"      -> SemiColon
        | "="      -> Equals
        | ","      -> Comma
        | "{"      -> OpenBrace
        | "}"      -> CloseBrace
        | "|>"     -> ForwardPipe
        | "<|"     -> BackwardPipe
        | "->"     -> ForwardArrow
        | "<-"     -> BackwardArrow
        | "/>"     -> XmlCloseTag
        | "type"   -> Type
        | "\r"     -> Return
        | _        -> Other
