module Hally.Alignment.Tests.TestTokens

open Hally.Alignment

open NUnit.Framework
open FsUnitTyped

[<TestCase( "\"" + ""             + "\""    , "\"\""            , null )>]
[<TestCase( "\"" + "A b\\\"\n cd" + "\" efg", "\"A b\\\"\n cd\"", "efg")>]
[<TestCase("$\"" + "{x}"          + "\""    , "$\"{x}\""        , null )>]
let ``Parsing.tryTokenize, when passed valid input, returns expected tokens`` (input : string) (expected0 : string) (expected1 : string) =
    let expected1 = Option.ofObj expected1

    match Parsing.tryTokenize input with
    | Result.Error (s, es) -> Assert.Fail($"{s}: {es}")
    | Result.Ok xs ->
        xs.[0].Value |> shouldEqual expected0
        match expected1 with
        | Some expected ->
            xs.[1].Value |> shouldEqual " "
            xs.[2].Value |> shouldEqual expected
            xs.Length |> shouldEqual 3
        | None ->
            xs.Length |> shouldEqual 1
