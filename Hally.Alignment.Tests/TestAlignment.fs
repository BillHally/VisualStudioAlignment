module Hally.Alignment.Tests.TestAlignment

open Hally.Alignment

open NUnit.Framework
open FsUnitTyped

[<RequireQualifiedAccess>]
module NoAlignmentRequired =
    let [<Literal>] Empty = ""

    let [<Literal>] OneLine = """
    let a = 3
"""

    let [<Literal>] WhiteSpaceOnly = """
    

    """

[<RequireQualifiedAccess>]
module AlignmentByEqualsRequired =
    let [<Literal>] NotContainingChar = """
    A : int
    AB : string
"""

    let [<Literal>] Unaligned00 = """
    let a = 3
    let ab = 4
"""

    let [<Literal>] Aligned00 = """
    let a  = 3
    let ab = 4
"""

    let [<Literal>] Unaligned01 = """
    let a = 3
    let ab = 4"""

    let [<Literal>] Aligned01 = """
    let a  = 3
    let ab = 4"""

    let [<Literal>] Unaligned02 = """
    let ab = 3
    let a = 4
"""

    let [<Literal>] Aligned02 = """
    let ab = 3
    let a  = 4
"""

    let [<Literal>] Unaligned03 = """
    let ab = 3
    let a = 4"""

    let [<Literal>] Aligned03 = """
    let ab = 3
    let a  = 4"""

    let [<Literal>] Unaligned04 = """
    let ab = 3

    let a = 4"""

    let [<Literal>] Aligned04 = """
    let ab = 3

    let a  = 4"""

[<TestCase(NoAlignmentRequired.Empty                  , NoAlignmentRequired.Empty                  )>]
[<TestCase(NoAlignmentRequired.OneLine                , NoAlignmentRequired.OneLine                )>]
[<TestCase(NoAlignmentRequired.WhiteSpaceOnly         , NoAlignmentRequired.WhiteSpaceOnly         )>]
[<TestCase(AlignmentByEqualsRequired.NotContainingChar, AlignmentByEqualsRequired.NotContainingChar)>]
[<TestCase(AlignmentByEqualsRequired.Unaligned00      , AlignmentByEqualsRequired.Aligned00        )>]
[<TestCase(AlignmentByEqualsRequired.Unaligned01      , AlignmentByEqualsRequired.Aligned01        )>]
[<TestCase(AlignmentByEqualsRequired.Unaligned02      , AlignmentByEqualsRequired.Aligned02        )>]
[<TestCase(AlignmentByEqualsRequired.Unaligned03      , AlignmentByEqualsRequired.Aligned03        )>]
[<TestCase(AlignmentByEqualsRequired.Unaligned04      , AlignmentByEqualsRequired.Aligned04        )>]
let ``Alignment.align, when passed '=', aligns the lines it is passed at that the first occurrence of that character`` (before : string) (after : string) =
    before
    |> Alignment.align '='
    |> shouldEqual after

[<TestCase(NoAlignmentRequired.Empty                  , NoAlignmentRequired.Empty                  )>]
[<TestCase(NoAlignmentRequired.OneLine                , NoAlignmentRequired.OneLine                )>]
[<TestCase(NoAlignmentRequired.WhiteSpaceOnly         , NoAlignmentRequired.WhiteSpaceOnly         )>]
[<TestCase(AlignmentByEqualsRequired.NotContainingChar, AlignmentByEqualsRequired.NotContainingChar)>]
[<TestCase(AlignmentByEqualsRequired.Aligned00        , AlignmentByEqualsRequired.Unaligned00      )>]
[<TestCase(AlignmentByEqualsRequired.Aligned01        , AlignmentByEqualsRequired.Unaligned01      )>]
[<TestCase(AlignmentByEqualsRequired.Aligned02        , AlignmentByEqualsRequired.Unaligned02      )>]
[<TestCase(AlignmentByEqualsRequired.Aligned03        , AlignmentByEqualsRequired.Unaligned03      )>]
[<TestCase(AlignmentByEqualsRequired.Aligned04        , AlignmentByEqualsRequired.Unaligned04      )>]
let ``Alignment.unalign, when passed '=', unaligns the lines it is passed at that the first occurrence of that character`` (before : string) (after : string) =
    before
    |> Alignment.unalign '='
    |> shouldEqual after

[<TestCase(NoAlignmentRequired.Empty                  , -1)>]
[<TestCase(NoAlignmentRequired.OneLine                , 10)>]
[<TestCase(AlignmentByEqualsRequired.NotContainingChar, -1)>]
[<TestCase(AlignmentByEqualsRequired.Unaligned00      , 11)>]
let ``getMaxFirstIndex always returns the expected index`` (x : string) (n : int) =
    x.Split('\n')
    |> Alignment.getMaxFirstIndex '='
    |> shouldEqual n

[<RequireQualifiedAccess>]
module AlignmentByEqualsAndColonRequired =
    let [<Literal>] Unaligned05 = """
    let ab : int = 3

    let a : int  = 4"""

    let [<Literal>] Aligned05 = """
    let ab : int = 3

    let a  : int = 4"""

[<TestCase(NoAlignmentRequired.Empty                    , NoAlignmentRequired.Empty                  )>]
[<TestCase(NoAlignmentRequired.OneLine                  , NoAlignmentRequired.OneLine                )>]
[<TestCase(NoAlignmentRequired.WhiteSpaceOnly           , NoAlignmentRequired.WhiteSpaceOnly         )>]
[<TestCase(AlignmentByEqualsRequired.Unaligned00        , AlignmentByEqualsRequired.Aligned00        )>]
[<TestCase(AlignmentByEqualsRequired.Unaligned01        , AlignmentByEqualsRequired.Aligned01        )>]
[<TestCase(AlignmentByEqualsRequired.Unaligned02        , AlignmentByEqualsRequired.Aligned02        )>]
[<TestCase(AlignmentByEqualsRequired.Unaligned03        , AlignmentByEqualsRequired.Aligned03        )>]
[<TestCase(AlignmentByEqualsRequired.Unaligned04        , AlignmentByEqualsRequired.Aligned04        )>]
[<TestCase(AlignmentByEqualsAndColonRequired.Unaligned05, AlignmentByEqualsAndColonRequired.Aligned05)>]
let ``Alignment.realignAll, always removes excess whitespace and aligns by both '=' and ':' characters`` (before : string) (after : string) =
    before
    |> Alignment.realignAll
    |> shouldEqual after
