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
let ``Alignment.align, when passed "=", aligns the lines it is passed at that the first occurrence of that character`` (before : string) (after : string) =
    before
    |> Alignment.align "="
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
let ``Alignment.unalign, unaligns the lines it is passed at that the first occurrence of that character`` (before : string) (after : string) =
    before
    |> Alignment.unalign
    |> shouldEqual after

[<TestCase(NoAlignmentRequired.Empty                  , -1)>]
[<TestCase(NoAlignmentRequired.OneLine                , 10)>]
[<TestCase(AlignmentByEqualsRequired.NotContainingChar, -1)>]
[<TestCase(AlignmentByEqualsRequired.Unaligned00      , 11)>]
let ``getMaxFirstIndex always returns the expected index`` (x : string) (n : int) =
    x.Split('\n')
    |> Alignment.getMaxFirstIndex "="
    |> shouldEqual n

[<RequireQualifiedAccess>]
module ComplexAlignmentRequired =
    let [<Literal>] Unaligned05 = """
    let ab : int = 3

    let a : int  = 4"""

    let [<Literal>] Aligned05 = """
    let ab : int = 3

    let a  : int = 4"""

    let [<Literal>] Unaligned06 = """
        interface IA with member    this.A    = this.A
"""

    let [<Literal>] Aligned06 = """
        interface IA with member this.A = this.A
"""

    let [<Literal>] Unaligned07 = """
        interface IA with member    this.A    = this.A
        interface IBc with member this.Bc        = this.Bc
"""

    let [<Literal>] Aligned07 = """
        interface IA  with member this.A  = this.A
        interface IBc with member this.Bc = this.Bc
"""

    let [<Literal>] Unaligned08 = """
        interface IA with member    this.A    =    this.A
        interface IBc with member this.Bc        =    this.Bc
        interface ID with member this.Defg =                this.Defg
"""

    let [<Literal>] Aligned08 = """
        interface IA  with member this.A    = this.A
        interface IBc with member this.Bc   = this.Bc
        interface ID  with member this.Defg = this.Defg
"""

    let [<Literal>] Unaligned09 = """
    type Abc =
        {
            A : int
            Bc : string
            Def :  float
            Gh :   Gh
            I :           int
        }
"""

    let [<Literal>] Aligned09 = """
    type Abc =
        {
            A   : int
            Bc  : string
            Def : float
            Gh  : Gh
            I   : int
        }
"""

[<TestCase(NoAlignmentRequired.Empty            , NoAlignmentRequired.Empty          )>]
[<TestCase(NoAlignmentRequired.OneLine          , NoAlignmentRequired.OneLine        )>]
[<TestCase(NoAlignmentRequired.WhiteSpaceOnly   , NoAlignmentRequired.WhiteSpaceOnly )>]
[<TestCase(AlignmentByEqualsRequired.Unaligned00, AlignmentByEqualsRequired.Aligned00)>]
[<TestCase(AlignmentByEqualsRequired.Unaligned01, AlignmentByEqualsRequired.Aligned01)>]
[<TestCase(AlignmentByEqualsRequired.Unaligned02, AlignmentByEqualsRequired.Aligned02)>]
[<TestCase(AlignmentByEqualsRequired.Unaligned03, AlignmentByEqualsRequired.Aligned03)>]
[<TestCase(AlignmentByEqualsRequired.Unaligned04, AlignmentByEqualsRequired.Aligned04)>]
[<TestCase(ComplexAlignmentRequired.Unaligned05 , ComplexAlignmentRequired.Aligned05 )>]
[<TestCase(ComplexAlignmentRequired.Unaligned06 , ComplexAlignmentRequired.Aligned06 )>]
[<TestCase(ComplexAlignmentRequired.Unaligned07 , ComplexAlignmentRequired.Aligned07 )>]
[<TestCase(ComplexAlignmentRequired.Unaligned08 , ComplexAlignmentRequired.Aligned08 )>]
[<TestCase(ComplexAlignmentRequired.Unaligned09 , ComplexAlignmentRequired.Aligned09 )>]
let ``Alignment.realignAll, always removes excess whitespace and aligns by both all required sub-strings`` (before : string) (after : string) =
    let actual = Alignment.realignAll before
    printfn $"Actual:\n{actual}"

    before
    |> Alignment.realignAll
    |> shouldEqual after
