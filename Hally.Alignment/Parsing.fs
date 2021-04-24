[<RequireQualifiedAccess>]
module Hally.Alignment.Parsing

open System
open System.Text

open FParsec

open Hally.Alignment

type private Tk =
    | S of string * int64
    | C of char   * int64
    | M of Tk list

[<RequireQualifiedAccess>]
module String =
    let ofList (xs : char list) =
        let builder = StringBuilder(xs.Length)
        for x in xs do
            builder.Append(x) |> ignore

        builder.ToString()

[<RequireQualifiedAccess>]
module private Tk =
    let tk2 tk0 tk1     = M [ tk0 ; tk1 ]
    let tk3 tk0 tk1 tk2 = M [ tk0 ; tk1; tk2 ]

    let rec toString = function
        | S  (x , _) -> x
        | C  (x , _) -> string x
        | M xs ->
            let sb = StringBuilder()
            for x in xs do sb.Append(toString x) |> ignore
            sb.ToString()

    let rec start = function
        | C (_, n) -> n
        | S (_, n) -> n
        | M  xs    -> start xs.[0]

// TODO: decide whether to keep this
type private Tok =
    {
        Tk        : Tk
        TokenKind : TokenKind
    }

    member this.Start = Tk.start this.Tk

[<RequireQualifiedAccess>]
module private Tok =
    let ofTk kind tk = { Tk = tk; TokenKind = kind }

    let toToken x =
        {
            Kind  = x.TokenKind
            Value = Tk.toString x.Tk
            Start = int x.Start
        }

// TODO: rename
let private withIndex (p : Parser<'r, unit>) (stream : CharStream<unit>) : Reply<'r * int64> =
    let start = stream.Index
    let reply = p stream

    match reply.Status with
    | ReplyStatus.Ok ->
        Reply((reply.Result, start))
    | _ ->
        Reply(reply.Status, reply.Error)

let private pchar'        x = withIndex (pchar        x)
let private pstring'      x = withIndex (pstring      x)
let private satisfy'      x = withIndex (satisfy      x)
let private noneOf'       x = withIndex (noneOf       x)
let private many1Satisfy' x = withIndex (many1Satisfy x)
let private many1'        x = withIndex (many1        x)
let private many1Chars'   x = withIndex (many1Chars   x)

let private literalStringPlain =
    let quote = pchar' '\"' |>> C

    let normalChar = satisfy' (fun c -> c <> '\\' && c <> '"') |>> C
    let escapedChar = pipe2 (pstring' "\\") (withIndex (anyOf "\\nrt\"" |>> string)) (fun (a, n) (b, _) -> (a + b), n) |>> S

    let manyChars = many (normalChar <|> escapedChar) |>> M

    pipe3 quote manyChars quote Tk.tk3

let private literalStringInterpolated = pipe2 (pchar' '$' |>> C) literalStringPlain Tk.tk2

let private tok kind (x : Reply<Tk>) : Reply<Tok> =
        match x.Status with
        | ReplyStatus.Ok -> Reply(Tok.ofTk kind x.Result)
        | _              -> Reply(x.Status, x.Error)

let private with'         : Parser<Tok, unit> = pstring' "with"   |>> S >> tok With
let private member'       : Parser<Tok, unit> = pstring' "member" |>> S >> tok Member
let private colon         : Parser<Tok, unit> = pchar'   ':'      |>> C >> tok Colon
let private semiColon     : Parser<Tok, unit> = pchar'   ';'      |>> C >> tok SemiColon
let private equals        : Parser<Tok, unit> = pchar'   '='      |>> C >> tok Equals
let private comma         : Parser<Tok, unit> = pchar'   ','      |>> C >> tok Comma
let private type'         : Parser<Tok, unit> = pstring' "type"   |>> S >> tok Type
let private openBrace     : Parser<Tok, unit> = pchar'   '{'      |>> C >> tok OpenBrace
let private closeBrace    : Parser<Tok, unit> = pchar'   '}'      |>> C >> tok CloseBrace
let private forwardPipe   : Parser<Tok, unit> = pstring' "|>"     |>> S >> tok ForwardPipe
let private backwardPipe  : Parser<Tok, unit> = pstring' "<|"     |>> S >> tok BackwardPipe
let private forwardArrow  : Parser<Tok, unit> = pstring' "->"     |>> S >> tok ForwardArrow
let private backwardArrow : Parser<Tok, unit> = pstring' "<-"     |>> S >> tok BackwardArrow
let private return'       : Parser<Tok, unit> = pchar'   '\r'     |>> C >> tok Return
let private xmlCloseTag   : Parser<Tok, unit> = pstring' "/>"     |>> S >> tok XmlCloseTag

let private whitespace : Parser<Tok, unit> = many1Chars' (pchar '\t' <|> pchar ' ') |>> S >> tok Whitespace

// TODO: Add support for verbatim and triple-quoted strings
let private str : Parser<Tok, unit> = (literalStringInterpolated <|> literalStringPlain) >> (tok LiteralString)

let private other      : Parser<Tok, unit> = many1Satisfy' (isNoneOf ":;=,{}|<>\"\n\r\t ") |>> S >> tok Other
let private otherLoose : Parser<Tok, unit> = many1Satisfy' (isNoneOf "\"\n\r\t "         ) |>> S >> tok Other

let private anyToken =
    with'
    <|> member'
    <|> colon
    <|> semiColon
    <|> equals
    <|> comma
    <|> type'
    <|> openBrace
    <|> closeBrace
    <|> forwardPipe
    <|> backwardPipe
    <|> forwardArrow
    <|> backwardArrow
    <|> str
    <|> xmlCloseTag
    <|> return'
    <|> whitespace
    <|> other      // Catch other things, but without greedily eating trailing comma etc.
    <|> otherLoose // Catch anything else // TODO: Add some property based tests to see what needs this

let private tokenize = many anyToken

module private ParserResult =
    let tryGetTokens = function
        | Success (xs : Tok list, (), _) ->
            xs
            |> List.map Tok.toToken
            |> Result.Ok
        | Failure (s, e, ()) -> Result.Error (s, e)

let tryTokenize x = run tokenize x |> ParserResult.tryGetTokens
