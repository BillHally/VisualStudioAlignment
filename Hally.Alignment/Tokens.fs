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
        $"%02d{this.Start}-%02d{this.Last} %-10s{value} {this.Kind}"

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
        | "type"   -> Type
        | "\r"     -> Return
        | _        -> Other
