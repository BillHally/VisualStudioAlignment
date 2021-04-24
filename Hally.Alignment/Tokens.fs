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
    | OpenParenthesis
    | CloseParenthesis
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
    // | .
    // Operators
    | XmlCloseTag
    | LineComment
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
        let value = this.Value.Replace('\r', '⏎').Replace('\n', '←')
        $"%03d{this.Start}-%03d{this.Last}:%17O{this.Kind}: %s{value}"

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
        OpenParenthesis
        CloseParenthesis
        ForwardPipe
        BackwardPipe
        ForwardArrow
        BackwardArrow
        LiteralString
        XmlCloseTag
        LineComment
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
        OpenParenthesis
        CloseParenthesis
        ForwardPipe
        BackwardPipe
        ForwardArrow
        BackwardArrow
        LiteralString
        XmlCloseTag
        LineComment
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
        | "("      -> OpenParenthesis
        | ")"      -> CloseParenthesis
        | "|>"     -> ForwardPipe
        | "<|"     -> BackwardPipe
        | "->"     -> ForwardArrow
        | "<-"     -> BackwardArrow
        | "/>"     -> XmlCloseTag
        | "type"   -> Type
        | "\r"     -> Return
        | _        -> Other
