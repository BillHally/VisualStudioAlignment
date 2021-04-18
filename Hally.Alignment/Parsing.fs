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
        | S  (x , n) -> x
        | C  (x , n) -> string x
        | M   xs     -> List.fold (fun acc x -> acc + toString x) "" xs

    let toToken kind index x =
        let value = toString x // TODO: 'x' holds 1 or more indices

        {
            Kind  = kind
            Value = value
            Start = index
            Last  = index + value.Length
        }

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

let private withIndex (p : Parser<'r, unit>) (stream : CharStream<unit>) : Reply<'r * int64> =
    let start = stream.Index
    let reply = p stream

    match reply.Status with
    | ReplyStatus.Ok -> Reply((reply.Result, start))
    | _              -> Reply(reply.Status, reply.Error)

let private pchar'        x = withIndex (pchar        x)
let private pstring'      x = withIndex (pstring      x)
let private satisfy'      x = withIndex (satisfy      x)
let private noneOf'       x = withIndex (noneOf       x)
let private many1Satisfy' x = withIndex (many1Satisfy x)
let private many1'        x = withIndex (many1        x)
let private many1Chars'   x = withIndex (many1Chars   x)

let private literalStringPlain =
    let normalChar  = satisfy' (fun c -> c <> '\\' && c <> '"') |>> C
    let escapedChar = pipe2 (pstring' "\\") (withIndex (anyOf "\\nrt\"" |>> string)) (fun (a, n) (b, _) -> (a + b), n) |>> S

    pipe3
        (pchar' '\"' |>> C)
        (many (normalChar <|> escapedChar) |>> M)
        (pchar' '\"' |>> C)
        (Tk.tk3)

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

let private whitespace : Parser<Tok, unit> = many1Chars' (pchar '\t' <|> pchar ' ') |>> S >> tok Whitespace

// TODO: Add support for verbatim and triple-quoted strings
let private str : Parser<Tok, unit> = (literalStringInterpolated <|> literalStringPlain) >> (tok LiteralString)

let private other      : Parser<Tok, unit> = many1Satisfy' (isNoneOf ":;=,{}|<>\"\n\r\t ") |>> S >> tok Other
let private otherLoose : Parser<Tok, unit> = many1Satisfy' (isNoneOf "\"\n\r\t ") |>> S >> tok Other

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
    <|> return'
    <|> whitespace
    //<|> space
    //<|> tab
    <|> other
    <|> otherLoose // Catch anything else

let private tokenize = many anyToken

module private ParserResult =
    let toToken x =
        let value = Tk.toString x.Tk
        let start = int x.Start

        match x.TokenKind with
        //| Tab
        //| Space -> None
        | _ ->
            Some
                {
                    Kind  = x.TokenKind
                    Value = value
                    Start = start
                    Last  = start + (value.Length - 1) // TODO: Do we need "Last"?
                }

    let tryGetTokens = function
        | Success (xs, u, p) ->
            let ts = xs |> List.choose toToken
            ts |> List.iter (fun x -> printfn $"Line:{x}")
            Result.Ok ts
        | Failure (x, u, p) -> Result.Error (x, u, p)

let tryTokenize x = run tokenize x |> ParserResult.tryGetTokens
