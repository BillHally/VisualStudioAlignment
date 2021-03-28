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

    let [<Literal>] Unaligned05 = """
        {
            D = efg 7
            Hi = jklmn 8 90
            O = pqr 1000 100 1
        }
"""

    let [<Literal>] Aligned05 = """
        {
            D  = efg 7
            Hi = jklmn 8 90
            O  = pqr 1000 100 1
        }
"""

[<RequireQualifiedAccess>]
module AlignmentByCommaRequired =
    let [<Literal>] Unaligned00 = """
    a, bc, def
    ghi, jklm, nop, q
"""

    let [<Literal>] Aligned00_00_08 = """
    a  , bc, def
    ghi, jklm, nop, q
"""

    let [<Literal>] Aligned00_00_16 = """
    a  , bc  , def
    ghi, jklm, nop, q
"""

    let [<Literal>] Aligned00_08_16 = """
    a, bc    , def
    ghi, jklm, nop, q
"""

[<TestCase(NoAlignmentRequired.Empty                  , "=", NoAlignmentRequired.Empty                  )>]
[<TestCase(NoAlignmentRequired.OneLine                , "=", NoAlignmentRequired.OneLine                )>]
[<TestCase(NoAlignmentRequired.WhiteSpaceOnly         , "=", NoAlignmentRequired.WhiteSpaceOnly         )>]
[<TestCase(AlignmentByEqualsRequired.NotContainingChar, "=", AlignmentByEqualsRequired.NotContainingChar)>]
[<TestCase(AlignmentByEqualsRequired.Unaligned00      , "=", AlignmentByEqualsRequired.Aligned00        )>]
[<TestCase(AlignmentByEqualsRequired.Unaligned01      , "=", AlignmentByEqualsRequired.Aligned01        )>]
[<TestCase(AlignmentByEqualsRequired.Unaligned02      , "=", AlignmentByEqualsRequired.Aligned02        )>]
[<TestCase(AlignmentByEqualsRequired.Unaligned03      , "=", AlignmentByEqualsRequired.Aligned03        )>]
[<TestCase(AlignmentByEqualsRequired.Unaligned04      , "=", AlignmentByEqualsRequired.Aligned04        )>]
//[<TestCase(AlignmentByEqualsRequired.Unaligned05      , "=", AlignmentByEqualsRequired.Aligned05        )>]
[<TestCase(AlignmentByCommaRequired.Unaligned00       , ",", AlignmentByCommaRequired.Aligned00_00_16   )>]
let ``Alignment.align, when passed a substring, aligns the lines it is passed at every occurrence of that substring`` (before : string) (substring : string) (after : string) =
    let actual = Alignment.align substring before
    printfn $"Before:\n012345678901234567890\n{before}"
    printfn $"Actual:\n012345678901234567890\n{actual}"

    actual |> shouldEqual after

[<TestCase(NoAlignmentRequired.Empty                  ,  0, 50, "=", NoAlignmentRequired.Empty                  , TestName = "Alignment.unalign: " + (nameof NoAlignmentRequired.Empty                  ))>]
[<TestCase(NoAlignmentRequired.OneLine                ,  0, 50, "=", NoAlignmentRequired.OneLine                , TestName = "Alignment.unalign: " + (nameof NoAlignmentRequired.OneLine                ))>]
[<TestCase(NoAlignmentRequired.WhiteSpaceOnly         ,  0, 50, "=", NoAlignmentRequired.WhiteSpaceOnly         , TestName = "Alignment.unalign: " + (nameof NoAlignmentRequired.WhiteSpaceOnly         ))>]
[<TestCase(AlignmentByEqualsRequired.NotContainingChar,  0, 50, "=", AlignmentByEqualsRequired.NotContainingChar, TestName = "Alignment.unalign: " + (nameof AlignmentByEqualsRequired.NotContainingChar))>]
[<TestCase(AlignmentByEqualsRequired.Aligned00        ,  0, 50, "=", AlignmentByEqualsRequired.Unaligned00      , TestName = "Alignment.unalign: " + (nameof AlignmentByEqualsRequired.Aligned00        ))>]
[<TestCase(AlignmentByEqualsRequired.Aligned01        ,  0, 50, "=", AlignmentByEqualsRequired.Unaligned01      , TestName = "Alignment.unalign: " + (nameof AlignmentByEqualsRequired.Aligned01        ))>]
[<TestCase(AlignmentByEqualsRequired.Aligned02        ,  0, 50, "=", AlignmentByEqualsRequired.Unaligned02      , TestName = "Alignment.unalign: " + (nameof AlignmentByEqualsRequired.Aligned02        ))>]
[<TestCase(AlignmentByEqualsRequired.Aligned03        ,  0, 50, "=", AlignmentByEqualsRequired.Unaligned03      , TestName = "Alignment.unalign: " + (nameof AlignmentByEqualsRequired.Aligned03        ))>]
[<TestCase(AlignmentByEqualsRequired.Aligned04        ,  0, 50, "=", AlignmentByEqualsRequired.Unaligned04      , TestName = "Alignment.unalign: " + (nameof AlignmentByEqualsRequired.Aligned04        ))>]
[<TestCase(AlignmentByCommaRequired.Aligned00_00_08   ,  0,  8, ",", AlignmentByCommaRequired.Unaligned00       , TestName = "Alignment.unalign: " + (nameof AlignmentByCommaRequired.Aligned00_00_08   ))>]
[<TestCase(AlignmentByCommaRequired.Aligned00_00_16   ,  0, 16, ",", AlignmentByCommaRequired.Unaligned00       , TestName = "Alignment.unalign: " + (nameof AlignmentByCommaRequired.Aligned00_00_16   ))>]
[<TestCase(AlignmentByCommaRequired.Aligned00_08_16   ,  8, 16, ",", AlignmentByCommaRequired.Unaligned00       , TestName = "Alignment.unalign: " + (nameof AlignmentByCommaRequired.Aligned00_08_16   ))>]
let ``Alignment.unalign, unaligns the lines it is passed from after the indent to the end of the line``
    (before : string) (startIndex : int) (endIndex : int) (s : string) (after : string) =
    before
    |> Alignment.unalign startIndex endIndex s
    |> shouldEqual after

[<TestCase(NoAlignmentRequired.Empty                  , 0, "=", -1)>]
[<TestCase(NoAlignmentRequired.OneLine                , 0, "=", 10)>]
[<TestCase(AlignmentByEqualsRequired.NotContainingChar, 0, "=", -1)>]
[<TestCase(AlignmentByEqualsRequired.Unaligned00      , 0, "=", 11)>]
[<TestCase(AlignmentByCommaRequired.Unaligned00       , 0, ",",  7)>]
[<TestCase(AlignmentByCommaRequired.Unaligned00       , 8, ",", 13)>]
let ``getMaxFirstIndex always returns the expected index`` (x : string) (startIndex : int) (alignBy : string) (expected : int) =
    x.Split('\n')
    |> Alignment.getMaxNextIndex startIndex alignBy
    |> shouldEqual expected

[<RequireQualifiedAccess>]
module ComplexAlignmentRequired =
    let [<Literal>] Unaligned05 = """
    let ab : int = 3

    let a : int  = 4"""

    let [<Literal>] Realigned05 = """
    let ab : int = 3

    let a  : int = 4"""

    let [<Literal>] Unaligned06 = """
        interface IA with member    this.A    = this.A
"""

    let [<Literal>] Realigned06 = """
        interface IA with member this.A = this.A
"""

    let [<Literal>] Unaligned07 = """
        interface IA with member    this.A    = this.A
        interface IBc with member this.Bc        = this.Bc
"""

    let [<Literal>] Realigned07 = """
        interface IA  with member this.A  = this.A
        interface IBc with member this.Bc = this.Bc
"""

    let [<Literal>] Unaligned08 = """
        interface IA with member    this.A    =    this.A
        interface IBc with member this.Bc        =    this.Bc
        interface ID with member this.Defg =                this.Defg
"""

    let [<Literal>] Realigned08 = """
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

    let [<Literal>] Realigned09 = """
    type Abc =
        {
            A   : int
            Bc  : string
            Def : float
            Gh  : Gh
            I   : int
        }
"""

    let [<Literal>] Unaligned10 = """
    let a, b          =   ABC
    let d ,  e   ,  f    ,   g    =       DEFGH
"""

    let [<Literal>] Realigned10 = """
    let a, b       = ABC
    let d, e, f, g = DEFGH
"""

    let [<Literal>] Unaligned11 = """
        {
            D = efg 7
            Hi  =  jklmn   8   90
            O  =  pqr 1000 100 1
        }
"""

    // Maintain existing spacing between unaffected non-whitespace substrings
    let [<Literal>] Realigned11 = """
        {
            D  = efg 7
            Hi =  jklmn   8   90
            O  =  pqr 1000 100 1
        }
"""

    // Re-align everything, including non-special cased non-whitespace substrings
    let [<Literal>] Realigned12 = """
        {
            D  = efg   7
            Hi = jklmn 8    90
            O  = pqr   1000 100 1
        }
"""

[<TestCase(NoAlignmentRequired.Empty            , NoAlignmentRequired.Empty           )>]
[<TestCase(NoAlignmentRequired.OneLine          , NoAlignmentRequired.OneLine         )>]
[<TestCase(NoAlignmentRequired.WhiteSpaceOnly   , NoAlignmentRequired.WhiteSpaceOnly  )>]
[<TestCase(AlignmentByEqualsRequired.Unaligned00, AlignmentByEqualsRequired.Aligned00 )>]
[<TestCase(AlignmentByEqualsRequired.Unaligned01, AlignmentByEqualsRequired.Aligned01 )>]
[<TestCase(AlignmentByEqualsRequired.Unaligned02, AlignmentByEqualsRequired.Aligned02 )>]
[<TestCase(AlignmentByEqualsRequired.Unaligned03, AlignmentByEqualsRequired.Aligned03 )>]
[<TestCase(AlignmentByEqualsRequired.Unaligned04, AlignmentByEqualsRequired.Aligned04 )>]
[<TestCase(ComplexAlignmentRequired.Unaligned05 , ComplexAlignmentRequired.Realigned05)>]
[<TestCase(ComplexAlignmentRequired.Unaligned06 , ComplexAlignmentRequired.Realigned06)>]
[<TestCase(ComplexAlignmentRequired.Unaligned07 , ComplexAlignmentRequired.Realigned07)>]
[<TestCase(ComplexAlignmentRequired.Unaligned08 , ComplexAlignmentRequired.Realigned08)>]
[<TestCase(ComplexAlignmentRequired.Unaligned09 , ComplexAlignmentRequired.Realigned09)>]
[<TestCase(ComplexAlignmentRequired.Unaligned10 , ComplexAlignmentRequired.Realigned10)>]
[<TestCase(ComplexAlignmentRequired.Unaligned11 , ComplexAlignmentRequired.Realigned11)>]
let ``Alignment.realignAll, always removes excess whitespace and aligns by all required sub-strings`` (before : string) (after : string) =
    let actual = Alignment.realignAll before
    printfn $"Before:\n012345678901234567890\n{before}"
    printfn $"Actual:\n012345678901234567890\n{actual}"

    actual |> shouldEqual after
