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
module AlignByEquals =
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
module AlignByComma =
    let [<Literal>] Comma_U_00 = """
    a, bc, def
    ghi, jklm, nop, q
"""

    let [<Literal>] Comma_A_00_00_08 = """
    a  , bc, def
    ghi, jklm, nop, q
"""

    let [<Literal>] Comma_A_00_00_16 = """
    a  , bc  , def
    ghi, jklm, nop, q
"""

    let [<Literal>] Comma_A_00_08_16 = """
    a, bc    , def
    ghi, jklm, nop, q
"""

[<RequireQualifiedAccess>]
module AlignByOther =
    let [<Literal>] Other_U_00 = """
    a bc def
    ghi jklm nop q
"""

    let [<Literal>] Other_A_00 = """
    a   bc   def
    ghi jklm nop q
"""

[<TestCase(NoAlignmentRequired.Empty         , "=", NoAlignmentRequired.Empty         , TestName = "Alignment.align " + (nameof NoAlignmentRequired.Empty         ))>]
[<TestCase(NoAlignmentRequired.OneLine       , "=", NoAlignmentRequired.OneLine       , TestName = "Alignment.align " + (nameof NoAlignmentRequired.OneLine       ))>]
[<TestCase(NoAlignmentRequired.WhiteSpaceOnly, "=", NoAlignmentRequired.WhiteSpaceOnly, TestName = "Alignment.align " + (nameof NoAlignmentRequired.WhiteSpaceOnly))>]
[<TestCase(AlignByEquals.NotContainingChar   , "=", AlignByEquals.NotContainingChar   , TestName = "Alignment.align " + (nameof AlignByEquals.NotContainingChar   ))>]
[<TestCase(AlignByEquals.Unaligned00         , "=", AlignByEquals.Aligned00           , TestName = "Alignment.align " + (nameof AlignByEquals.Unaligned00         ))>]
[<TestCase(AlignByEquals.Unaligned01         , "=", AlignByEquals.Aligned01           , TestName = "Alignment.align " + (nameof AlignByEquals.Unaligned01         ))>]
[<TestCase(AlignByEquals.Unaligned02         , "=", AlignByEquals.Aligned02           , TestName = "Alignment.align " + (nameof AlignByEquals.Unaligned02         ))>]
[<TestCase(AlignByEquals.Unaligned03         , "=", AlignByEquals.Aligned03           , TestName = "Alignment.align " + (nameof AlignByEquals.Unaligned03         ))>]
[<TestCase(AlignByEquals.Unaligned04         , "=", AlignByEquals.Aligned04           , TestName = "Alignment.align " + (nameof AlignByEquals.Unaligned04         ))>]
[<TestCase(AlignByEquals.Unaligned05         , "=", AlignByEquals.Aligned05           , TestName = "Alignment.align " + (nameof AlignByEquals.Unaligned05         ))>]
[<TestCase(AlignByComma.Comma_U_00           , ",", AlignByComma.Comma_A_00_00_16     , TestName = "Alignment.align " + (nameof AlignByComma.Comma_U_00           ))>]
[<TestCase(AlignByOther.Other_U_00           , "" , AlignByOther.Other_A_00           , TestName = "Alignment.align " + (nameof AlignByOther.Other_U_00           ))>]
let ``Alignment.align, when passed a substring, aligns the lines it is passed at every occurrence of that substring`` (before : string) (substring : string) (after : string) =
    let actual = Alignment.align substring before

    printfn $"Before  :\n012345678901234567890\n{before}"
    printfn $"Expected:\n012345678901234567890\n{after}"
    printfn $"Actual  :\n012345678901234567890\n{actual}"

    actual |> shouldEqual after

[<TestCase(NoAlignmentRequired.Empty         ,  0, 50, "=", NoAlignmentRequired.Empty         , TestName = "Alignment.unalign: " + (nameof NoAlignmentRequired.Empty         ))>]
[<TestCase(NoAlignmentRequired.OneLine       ,  0, 50, "=", NoAlignmentRequired.OneLine       , TestName = "Alignment.unalign: " + (nameof NoAlignmentRequired.OneLine       ))>]
[<TestCase(NoAlignmentRequired.WhiteSpaceOnly,  0, 50, "=", NoAlignmentRequired.WhiteSpaceOnly, TestName = "Alignment.unalign: " + (nameof NoAlignmentRequired.WhiteSpaceOnly))>]
[<TestCase(AlignByEquals.NotContainingChar   ,  0, 50, "=", AlignByEquals.NotContainingChar   , TestName = "Alignment.unalign: " + (nameof AlignByEquals.NotContainingChar   ))>]
[<TestCase(AlignByEquals.Aligned00           ,  0, 50, "=", AlignByEquals.Unaligned00         , TestName = "Alignment.unalign: " + (nameof AlignByEquals.Aligned00           ))>]
[<TestCase(AlignByEquals.Aligned01           ,  0, 50, "=", AlignByEquals.Unaligned01         , TestName = "Alignment.unalign: " + (nameof AlignByEquals.Aligned01           ))>]
[<TestCase(AlignByEquals.Aligned02           ,  0, 50, "=", AlignByEquals.Unaligned02         , TestName = "Alignment.unalign: " + (nameof AlignByEquals.Aligned02           ))>]
[<TestCase(AlignByEquals.Aligned03           ,  0, 50, "=", AlignByEquals.Unaligned03         , TestName = "Alignment.unalign: " + (nameof AlignByEquals.Aligned03           ))>]
[<TestCase(AlignByEquals.Aligned04           ,  0, 50, "=", AlignByEquals.Unaligned04         , TestName = "Alignment.unalign: " + (nameof AlignByEquals.Aligned04           ))>]
[<TestCase(AlignByComma.Comma_A_00_00_08     ,  0,  8, ",", AlignByComma.Comma_U_00           , TestName = "Alignment.unalign: " + (nameof AlignByComma.Comma_A_00_00_08     ))>]
[<TestCase(AlignByComma.Comma_A_00_00_16     ,  0, 16, ",", AlignByComma.Comma_U_00           , TestName = "Alignment.unalign: " + (nameof AlignByComma.Comma_A_00_00_16     ))>]
[<TestCase(AlignByComma.Comma_A_00_08_16     ,  8, 16, ",", AlignByComma.Comma_U_00           , TestName = "Alignment.unalign: " + (nameof AlignByComma.Comma_A_00_08_16     ))>]
let ``Alignment.unalign, unaligns the lines it is passed from after the indent to the end of the line``
    (before : string) (startIndex : int) (endIndex : int) (s : string) (after : string) =
    before
    |> Alignment.unalign startIndex endIndex s
    |> shouldEqual after

[<TestCase(NoAlignmentRequired.Empty      , 0, "=", "-1"         , TestName = "Alignment.getNextIndices " + (nameof NoAlignmentRequired.Empty      ) + ", {1}, {2}")>]
[<TestCase(NoAlignmentRequired.OneLine    , 0, "=", "-1;10;-1"   , TestName = "Alignment.getNextIndices " + (nameof NoAlignmentRequired.OneLine    ) + ", {1}, {2}")>]
[<TestCase(AlignByEquals.NotContainingChar, 0, "=", "-1;-1;-1;-1", TestName = "Alignment.getNextIndices " + (nameof AlignByEquals.NotContainingChar) + ", {1}, {2}")>]
[<TestCase(AlignByEquals.Unaligned00      , 0, "=", "-1;10;11;-1", TestName = "Alignment.getNextIndices " + (nameof AlignByEquals.Unaligned00      ) + ", {1}, {2}")>]
[<TestCase(AlignByComma.Comma_U_00        , 0, ",", "-1;5;7;-1"  , TestName = "Alignment.getNextIndices " + (nameof AlignByComma.Comma_U_00        ) + ", {1}, {2}")>]
[<TestCase(AlignByComma.Comma_U_00        , 8, ",", "-1;9;13;-1" , TestName = "Alignment.getNextIndices " + (nameof AlignByComma.Comma_U_00        ) + ", {1}, {2}")>]
[<TestCase(AlignByOther.Other_U_00        , 0, "" ,  "-1;4;4;-1" , TestName = "Alignment.getNextIndices " + (nameof AlignByOther.Other_U_00        ) + ", {1}, {2}")>]
[<TestCase(AlignByOther.Other_U_00        , 5, "" ,  "-1;6;8;-1" , TestName = "Alignment.getNextIndices " + (nameof AlignByOther.Other_U_00        ) + ", {1}, {2}")>]
let ``Alignment.getNextIndices always returns the expected index`` (x : string) (startIndex : int) (alignBy : string) (expected : string) =
    let expected = expected.Split(";") |> Array.map (fun x -> [| int x |])

    x.Split('\n')
    |> Alignment.getNextIndices startIndex [| TokenKind.ofString alignBy |]
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
        {
            A : int
            Bc : string
            Def :  float
            Gh :   Gh
            I :           int
        }
"""

    let [<Literal>] Realigned09 = """
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
            D = efg 7
            Hi  =  jklmn   8   90
            O  =  pqr 1000 100 1
"""

    // Re-align everything, including non-special cased non-whitespace substrings
    let [<Literal>] Realigned11 = """
            D  = efg   7
            Hi = jklmn 8    90
            O  = pqr   1000 100 1
"""

[<TestCase(NoAlignmentRequired.Empty           , NoAlignmentRequired.Empty           , TestName = "Alignment.realignAll " + (nameof NoAlignmentRequired.Empty           ))>]
[<TestCase(NoAlignmentRequired.OneLine         , NoAlignmentRequired.OneLine         , TestName = "Alignment.realignAll " + (nameof NoAlignmentRequired.OneLine         ))>]
[<TestCase(NoAlignmentRequired.WhiteSpaceOnly  , NoAlignmentRequired.WhiteSpaceOnly  , TestName = "Alignment.realignAll " + (nameof NoAlignmentRequired.WhiteSpaceOnly  ))>]
[<TestCase(AlignByEquals.Unaligned00           , AlignByEquals.Aligned00             , TestName = "Alignment.realignAll " + (nameof AlignByEquals.Unaligned00           ))>]
[<TestCase(AlignByEquals.Unaligned01           , AlignByEquals.Aligned01             , TestName = "Alignment.realignAll " + (nameof AlignByEquals.Unaligned01           ))>]
[<TestCase(AlignByEquals.Unaligned02           , AlignByEquals.Aligned02             , TestName = "Alignment.realignAll " + (nameof AlignByEquals.Unaligned02           ))>]
[<TestCase(AlignByEquals.Unaligned03           , AlignByEquals.Aligned03             , TestName = "Alignment.realignAll " + (nameof AlignByEquals.Unaligned03           ))>]
[<TestCase(AlignByEquals.Unaligned04           , AlignByEquals.Aligned04             , TestName = "Alignment.realignAll " + (nameof AlignByEquals.Unaligned04           ))>]
[<TestCase(ComplexAlignmentRequired.Unaligned05, ComplexAlignmentRequired.Realigned05, TestName = "Alignment.realignAll " + (nameof ComplexAlignmentRequired.Unaligned05))>]
[<TestCase(ComplexAlignmentRequired.Unaligned06, ComplexAlignmentRequired.Realigned06, TestName = "Alignment.realignAll " + (nameof ComplexAlignmentRequired.Unaligned06))>]
[<TestCase(ComplexAlignmentRequired.Unaligned07, ComplexAlignmentRequired.Realigned07, TestName = "Alignment.realignAll " + (nameof ComplexAlignmentRequired.Unaligned07))>]
[<TestCase(ComplexAlignmentRequired.Unaligned08, ComplexAlignmentRequired.Realigned08, TestName = "Alignment.realignAll " + (nameof ComplexAlignmentRequired.Unaligned08))>]
[<TestCase(ComplexAlignmentRequired.Unaligned09, ComplexAlignmentRequired.Realigned09, TestName = "Alignment.realignAll " + (nameof ComplexAlignmentRequired.Unaligned09))>]
[<TestCase(ComplexAlignmentRequired.Unaligned10, ComplexAlignmentRequired.Realigned10, TestName = "Alignment.realignAll " + (nameof ComplexAlignmentRequired.Unaligned10))>]
[<TestCase(ComplexAlignmentRequired.Unaligned11, ComplexAlignmentRequired.Realigned11, TestName = "Alignment.realignAll " + (nameof ComplexAlignmentRequired.Unaligned11))>]
let ``Alignment.realignAll, always removes excess whitespace and aligns by all required sub-strings`` (before : string) (after : string) =
    let actual = Alignment.realignAll before
    printfn $"Before  :\n012345678901234567890\n{before}"
    printfn $"Expected:\n012345678901234567890\n{after}"
    printfn $"Actual  :\n012345678901234567890\n{actual}"

    actual |> shouldEqual after
