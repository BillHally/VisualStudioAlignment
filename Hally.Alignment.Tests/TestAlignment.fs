module Hally.Alignment.Tests.TestAlignment

open Hally.Alignment

open NUnit.Framework
open FsUnitTyped

let dump before actual expected =
    printfn $"Before:"
    printfn $"000000000011111111112222222222333333333344444444445"
    printfn $"012345678901234567890123456789012345678901234567890"
    printfn $"{before}"
    printfn $"Actual  :"
    printfn $"000000000011111111112222222222333333333344444444445"
    printfn $"012345678901234567890123456789012345678901234567890"
    printfn $"{actual}"
    printfn $"Expected:"
    printfn $"000000000011111111112222222222333333333344444444445"
    printfn $"012345678901234567890123456789012345678901234567890"
    printfn $"{expected}"

[<RequireQualifiedAccess>]
module NoAlignmentRequired =
    let [<Literal>] Empty = ""

    let [<Literal>] OneLine = """
    let a = 3
"""

    let [<Literal>] WhiteSpaceOnly = """
    

    """

    let [<Literal>] WhiteSpaceOnly_Unaligned = """


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

[<RequireQualifiedAccess>]
module Xml =
    let [<Literal>] Before00    = """
        <IDSymbol   name="AlignmentMenu"      value="0x2000"         />
        <IDSymbol     name="AlignmentMenuGroup" value="0x2100"         />
"""

    let [<Literal>] Unaligned00 = """
        <IDSymbol name="AlignmentMenu" value="0x2000" />
        <IDSymbol name="AlignmentMenuGroup" value="0x2100" />
"""

    let [<Literal>] Realigned00 = """
        <IDSymbol name = "AlignmentMenu"      value="0x2000" />
        <IDSymbol name = "AlignmentMenuGroup" value="0x2100" />
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

    //dump before actual after

    actual |> shouldEqual after

[<TestCase(NoAlignmentRequired.Empty         , NoAlignmentRequired.Empty                   , TestName = "Alignment.unalign: " + (nameof NoAlignmentRequired) + "." + (nameof NoAlignmentRequired.Empty         ))>]
[<TestCase(NoAlignmentRequired.OneLine       , NoAlignmentRequired.OneLine                 , TestName = "Alignment.unalign: " + (nameof NoAlignmentRequired) + "." + (nameof NoAlignmentRequired.OneLine       ))>]
[<TestCase(NoAlignmentRequired.WhiteSpaceOnly, NoAlignmentRequired.WhiteSpaceOnly_Unaligned, TestName = "Alignment.unalign: " + (nameof NoAlignmentRequired) + "." + (nameof NoAlignmentRequired.WhiteSpaceOnly))>]
[<TestCase(AlignByEquals.NotContainingChar   , AlignByEquals.NotContainingChar             , TestName = "Alignment.unalign: " + (nameof AlignByEquals      ) + "." + (nameof AlignByEquals.NotContainingChar   ))>]
[<TestCase(AlignByEquals.Aligned00           , AlignByEquals.Unaligned00                   , TestName = "Alignment.unalign: " + (nameof AlignByEquals      ) + "." + (nameof AlignByEquals.Aligned00           ))>]
[<TestCase(AlignByEquals.Aligned01           , AlignByEquals.Unaligned01                   , TestName = "Alignment.unalign: " + (nameof AlignByEquals      ) + "." + (nameof AlignByEquals.Aligned01           ))>]
[<TestCase(AlignByEquals.Aligned02           , AlignByEquals.Unaligned02                   , TestName = "Alignment.unalign: " + (nameof AlignByEquals      ) + "." + (nameof AlignByEquals.Aligned02           ))>]
[<TestCase(AlignByEquals.Aligned03           , AlignByEquals.Unaligned03                   , TestName = "Alignment.unalign: " + (nameof AlignByEquals      ) + "." + (nameof AlignByEquals.Aligned03           ))>]
[<TestCase(AlignByEquals.Aligned04           , AlignByEquals.Unaligned04                   , TestName = "Alignment.unalign: " + (nameof AlignByEquals      ) + "." + (nameof AlignByEquals.Aligned04           ))>]
[<TestCase(AlignByComma.Comma_A_00_00_08     , AlignByComma.Comma_U_00                     , TestName = "Alignment.unalign: " + (nameof AlignByComma       ) + "." + (nameof AlignByComma.Comma_A_00_00_08     ))>]
[<TestCase(AlignByComma.Comma_A_00_00_16     , AlignByComma.Comma_U_00                     , TestName = "Alignment.unalign: " + (nameof AlignByComma       ) + "." + (nameof AlignByComma.Comma_A_00_00_16     ))>]
[<TestCase(AlignByComma.Comma_A_00_08_16     , AlignByComma.Comma_U_00                     , TestName = "Alignment.unalign: " + (nameof AlignByComma       ) + "." + (nameof AlignByComma.Comma_A_00_08_16     ))>]
[<TestCase(Xml.Before00                      , Xml.Unaligned00                             , TestName = "Alignment.unalign: " + (nameof Xml                ) + "." + (nameof Xml.Before00                      ))>]
let ``Alignment.unalign, unaligns the lines it is passed from after the indent to the end of the line`` (before : string) (after : string) =
    let actual = before |> Alignment.unalign

    //dump before actual after

    actual |> shouldEqual after

[<TestCase(NoAlignmentRequired.Empty      , 0, "=", "-1"         , TestName = "Alignment.getNextIndices " + (nameof NoAlignmentRequired.Empty      ) + ", {1}, {2}")>]
[<TestCase(NoAlignmentRequired.OneLine    , 0, "=", "-1;10;-1"   , TestName = "Alignment.getNextIndices " + (nameof NoAlignmentRequired.OneLine    ) + ", {1}, {2}")>]
[<TestCase(AlignByEquals.NotContainingChar, 0, "=", "-1;-1;-1;-1", TestName = "Alignment.getNextIndices " + (nameof AlignByEquals.NotContainingChar) + ", {1}, {2}")>]
[<TestCase(AlignByEquals.Unaligned00      , 0, "=", "-1;10;11;-1", TestName = "Alignment.getNextIndices " + (nameof AlignByEquals.Unaligned00      ) + ", {1}, {2}")>]
[<TestCase(AlignByComma.Comma_U_00        , 0, ",", "-1;5;7;-1"  , TestName = "Alignment.getNextIndices " + (nameof AlignByComma.Comma_U_00        ) + ", {1}, {2}")>]
[<TestCase(AlignByComma.Comma_U_00        , 8, ",", "-1;9;13;-1" , TestName = "Alignment.getNextIndices " + (nameof AlignByComma.Comma_U_00        ) + ", {1}, {2}")>]
[<TestCase(AlignByOther.Other_U_00        , 0, "" ,  "-1;4;4;-1" , TestName = "Alignment.getNextIndices " + (nameof AlignByOther.Other_U_00        ) + ", {1}, {2}")>]
[<TestCase(AlignByOther.Other_U_00        , 5, "" ,  "-1;6;8;-1" , TestName = "Alignment.getNextIndices " + (nameof AlignByOther.Other_U_00        ) + ", {1}, {2}")>]
let ``Alignment.getNextIndices always returns the expected index`` (before : string) (startIndex : int) (alignBy : string) (expected : string) =

    let actual =
        before.Split('\n')
        |> Array.map Line.ofString
        |> Alignment.getNextIndices startIndex [| TokenKind.ofString alignBy |]

    //dump before actual expected

    let expected = expected.Split(';') |> Array.map (fun x -> [| int x |])

    actual |> shouldEqual expected

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
    let [<Literal>] Realigned11_A = """
            D  = efg   7
            Hi = jklmn 8    90
            O  = pqr   1000 100 1
"""

    // Re-align everything, excluding non-special cased non-whitespace substrings
    let [<Literal>] Realigned11_B = """
            D  = efg 7
            Hi = jklmn 8 90
            O  = pqr 1000 100 1
"""

    let [<Literal>] Unaligned12 = """
        let         thing3         arg0        arg1      =     thing0    arg0   arg1  |> thing1 other0       |> thing2
        let thing4 arg0 = thing0 arg0 other0 |> thing1  whatever0 |>     thing2
"""

    let [<Literal>] Realigned12 = """
        let thing3 arg0 arg1 = thing0 arg0 arg1   |> thing1 other0    |> thing2
        let thing4 arg0      = thing0 arg0 other0 |> thing1 whatever0 |> thing2
"""

    let [<Literal>] Unaligned13 = """
        let abc <- def
        let gh, ijk <- 5, 700
"""

    let [<Literal>] Realigned13 = """
        let abc     <- def
        let gh, ijk <- 5, 700
"""

    let [<Literal>] Unaligned14 = """
        type Abc = { Def : G; Hijklm : Nop } with member this.Qrstu () = $"{this.Def} ({this.Hijklm})"
        type Ab     = { D : Efg; Hij : Klm } with   member this.Nopq = 8
"""

    let [<Literal>] Realigned14 = """
        type Abc = { Def : G  ; Hijklm : Nop } with member this.Qrstu () = $"{this.Def} ({this.Hijklm})"
        type Ab  = { D   : Efg; Hij    : Klm } with member this.Nopq     = 8
"""


    let [<Literal>] Unaligned15 = """
        type Abc = "{ Def : G; Hijklm : Nop } with member this.Qrstu () = $\"{this.Def} ({this.Hijklm})\""
        type Ab     = { D : Efg; Hij : Klm } with   member this.Nopq = 8
"""

    let [<Literal>] Realigned15 = """
        type Abc = "{ Def : G; Hijklm : Nop } with member this.Qrstu () = $\"{this.Def} ({this.Hijklm})\""
        type Ab  = { D : Efg; Hij : Klm } with member this.Nopq = 8
"""

    let[<Literal>] Unaligned16 = """
        | Abcde _ -> AbcdeThing
        | AbcdefGhijk _ -> AbcdefGhijkThing
        | A1bcd (_, x) -> A1bcdThing x
"""

    let[<Literal>] Realigned16 = """
        | Abcde       _     -> AbcdeThing
        | AbcdefGhijk _     -> AbcdefGhijkThing
        | A1bcd      (_, x) -> A1bcdThing x
"""


    let[<Literal>] Unaligned17 = """
        | Abcde _ -> AbcdeThing
        | AbcdefGhijk (_, x) -> AbcdefGhijkThing
        | A1bcd x -> A1bcdThing x
"""

    let[<Literal>] Realigned17 = """
        | Abcde        _     -> AbcdeThing
        | AbcdefGhijk (_, x) -> AbcdefGhijkThing
        | A1bcd        x     -> A1bcdThing x
"""

[<TestCase(NoAlignmentRequired.Empty           , NoAlignmentRequired.Empty             , TestName = "{m}: " + (nameof NoAlignmentRequired.Empty           ))>]
[<TestCase(NoAlignmentRequired.OneLine         , NoAlignmentRequired.OneLine           , TestName = "{m}: " + (nameof NoAlignmentRequired.OneLine         ))>]
[<TestCase(NoAlignmentRequired.WhiteSpaceOnly  , NoAlignmentRequired.WhiteSpaceOnly_Unaligned, TestName = "{m}: " + (nameof NoAlignmentRequired.WhiteSpaceOnly  ))>]
[<TestCase(AlignByEquals.Unaligned00           , AlignByEquals.Aligned00               , TestName = "{m}: " + (nameof AlignByEquals.Unaligned00           ))>]
[<TestCase(AlignByEquals.Unaligned01           , AlignByEquals.Aligned01               , TestName = "{m}: " + (nameof AlignByEquals.Unaligned01           ))>]
[<TestCase(AlignByEquals.Unaligned02           , AlignByEquals.Aligned02               , TestName = "{m}: " + (nameof AlignByEquals.Unaligned02           ))>]
[<TestCase(AlignByEquals.Unaligned03           , AlignByEquals.Aligned03               , TestName = "{m}: " + (nameof AlignByEquals.Unaligned03           ))>]
[<TestCase(AlignByEquals.Unaligned04           , AlignByEquals.Aligned04               , TestName = "{m}: " + (nameof AlignByEquals.Unaligned04           ))>]
[<TestCase(ComplexAlignmentRequired.Unaligned05, ComplexAlignmentRequired.Realigned05  , TestName = "{m}: " + (nameof ComplexAlignmentRequired.Unaligned05))>]
[<TestCase(ComplexAlignmentRequired.Unaligned06, ComplexAlignmentRequired.Realigned06  , TestName = "{m}: " + (nameof ComplexAlignmentRequired.Unaligned06))>]
[<TestCase(ComplexAlignmentRequired.Unaligned07, ComplexAlignmentRequired.Realigned07  , TestName = "{m}: " + (nameof ComplexAlignmentRequired.Unaligned07))>]
[<TestCase(ComplexAlignmentRequired.Unaligned08, ComplexAlignmentRequired.Realigned08  , TestName = "{m}: " + (nameof ComplexAlignmentRequired.Unaligned08))>]
[<TestCase(ComplexAlignmentRequired.Unaligned09, ComplexAlignmentRequired.Realigned09  , TestName = "{m}: " + (nameof ComplexAlignmentRequired.Unaligned09))>]
[<TestCase(ComplexAlignmentRequired.Unaligned10, ComplexAlignmentRequired.Realigned10  , TestName = "{m}: " + (nameof ComplexAlignmentRequired.Unaligned10))>]
[<TestCase(ComplexAlignmentRequired.Unaligned11, ComplexAlignmentRequired.Realigned11_A, TestName = "{m}: " + (nameof ComplexAlignmentRequired.Unaligned11))>]
[<TestCase(ComplexAlignmentRequired.Unaligned12, ComplexAlignmentRequired.Realigned12  , TestName = "{m}: " + (nameof ComplexAlignmentRequired.Unaligned12))>]
[<TestCase(ComplexAlignmentRequired.Unaligned13, ComplexAlignmentRequired.Realigned13  , TestName = "{m}: " + (nameof ComplexAlignmentRequired.Unaligned13))>]
[<TestCase(ComplexAlignmentRequired.Unaligned14, ComplexAlignmentRequired.Realigned14  , TestName = "{m}: " + (nameof ComplexAlignmentRequired.Unaligned14))>]
[<TestCase(ComplexAlignmentRequired.Unaligned15, ComplexAlignmentRequired.Realigned15  , TestName = "{m}: " + (nameof ComplexAlignmentRequired.Unaligned15))>]
[<TestCase(ComplexAlignmentRequired.Unaligned16, ComplexAlignmentRequired.Realigned16  , TestName = "{m}: " + (nameof ComplexAlignmentRequired.Unaligned16))>]
[<TestCase(ComplexAlignmentRequired.Unaligned17, ComplexAlignmentRequired.Realigned17  , TestName = "{m}: " + (nameof ComplexAlignmentRequired.Unaligned17))>]
let ``Alignment.realignAllExtended, always removes excess whitespace and aligns by all required token kinds`` (before : string) (after : string) =
    let actual =
        before.Split('\n')
        |> Alignment.realignAllExtended
        |> String.concat "\n"

    //dump before actual after

    actual |> shouldEqual after

[<TestCase(NoAlignmentRequired.Empty           , NoAlignmentRequired.Empty             , TestName = "{m}: " + (nameof NoAlignmentRequired.Empty           ))>]
[<TestCase(NoAlignmentRequired.OneLine         , NoAlignmentRequired.OneLine           , TestName = "{m}: " + (nameof NoAlignmentRequired.OneLine         ))>]
[<TestCase(NoAlignmentRequired.WhiteSpaceOnly  , NoAlignmentRequired.WhiteSpaceOnly_Unaligned, TestName = "{m}: " + (nameof NoAlignmentRequired.WhiteSpaceOnly  ))>]
[<TestCase(AlignByEquals.Unaligned00           , AlignByEquals.Aligned00               , TestName = "{m}: " + (nameof AlignByEquals.Unaligned00           ))>]
[<TestCase(AlignByEquals.Unaligned01           , AlignByEquals.Aligned01               , TestName = "{m}: " + (nameof AlignByEquals.Unaligned01           ))>]
[<TestCase(AlignByEquals.Unaligned02           , AlignByEquals.Aligned02               , TestName = "{m}: " + (nameof AlignByEquals.Unaligned02           ))>]
[<TestCase(AlignByEquals.Unaligned03           , AlignByEquals.Aligned03               , TestName = "{m}: " + (nameof AlignByEquals.Unaligned03           ))>]
[<TestCase(AlignByEquals.Unaligned04           , AlignByEquals.Aligned04               , TestName = "{m}: " + (nameof AlignByEquals.Unaligned04           ))>]
[<TestCase(ComplexAlignmentRequired.Unaligned05, ComplexAlignmentRequired.Realigned05  , TestName = "{m}: " + (nameof ComplexAlignmentRequired.Unaligned05))>]
[<TestCase(ComplexAlignmentRequired.Unaligned06, ComplexAlignmentRequired.Realigned06  , TestName = "{m}: " + (nameof ComplexAlignmentRequired.Unaligned06))>]
[<TestCase(ComplexAlignmentRequired.Unaligned07, ComplexAlignmentRequired.Realigned07  , TestName = "{m}: " + (nameof ComplexAlignmentRequired.Unaligned07))>]
[<TestCase(ComplexAlignmentRequired.Unaligned08, ComplexAlignmentRequired.Realigned08  , TestName = "{m}: " + (nameof ComplexAlignmentRequired.Unaligned08))>]
[<TestCase(ComplexAlignmentRequired.Unaligned09, ComplexAlignmentRequired.Realigned09  , TestName = "{m}: " + (nameof ComplexAlignmentRequired.Unaligned09))>]
[<TestCase(ComplexAlignmentRequired.Unaligned10, ComplexAlignmentRequired.Realigned10  , TestName = "{m}: " + (nameof ComplexAlignmentRequired.Unaligned10))>]
[<TestCase(ComplexAlignmentRequired.Unaligned11, ComplexAlignmentRequired.Realigned11_B, TestName = "{m}: " + (nameof ComplexAlignmentRequired.Unaligned11))>]
let ``Alignment.realignAll, always removes excess whitespace and aligns by all required token kinds`` (before : string) (after : string) =
    let actual =
        before.Split('\n')
        |> Alignment.realignAll
        |> String.concat "\n"

    //dump before actual after

    actual |> shouldEqual after

[<TestCase(AlignByComma.Comma_A_00_00_08       , TestName = "{m}: " + (nameof AlignByComma            ) + "." + (nameof AlignByComma.Comma_A_00_00_08       ))>]
[<TestCase(AlignByComma.Comma_A_00_00_16       , TestName = "{m}: " + (nameof AlignByComma            ) + "." + (nameof AlignByComma.Comma_A_00_00_16       ))>]
[<TestCase(AlignByComma.Comma_A_00_08_16       , TestName = "{m}: " + (nameof AlignByComma            ) + "." + (nameof AlignByComma.Comma_A_00_08_16       ))>]
[<TestCase(AlignByComma.Comma_U_00             , TestName = "{m}: " + (nameof AlignByComma            ) + "." + (nameof AlignByComma.Comma_U_00             ))>]
[<TestCase(AlignByEquals.Aligned00             , TestName = "{m}: " + (nameof AlignByEquals           ) + "." + (nameof AlignByEquals.Aligned00             ))>]
[<TestCase(AlignByEquals.Aligned01             , TestName = "{m}: " + (nameof AlignByEquals           ) + "." + (nameof AlignByEquals.Aligned01             ))>]
[<TestCase(AlignByEquals.Aligned02             , TestName = "{m}: " + (nameof AlignByEquals           ) + "." + (nameof AlignByEquals.Aligned02             ))>]
[<TestCase(AlignByEquals.Aligned03             , TestName = "{m}: " + (nameof AlignByEquals           ) + "." + (nameof AlignByEquals.Aligned03             ))>]
[<TestCase(AlignByEquals.Aligned04             , TestName = "{m}: " + (nameof AlignByEquals           ) + "." + (nameof AlignByEquals.Aligned04             ))>]
[<TestCase(AlignByEquals.NotContainingChar     , TestName = "{m}: " + (nameof AlignByEquals           ) + "." + (nameof AlignByEquals.NotContainingChar     ))>]
[<TestCase(AlignByEquals.Unaligned00           , TestName = "{m}: " + (nameof AlignByEquals           ) + "." + (nameof AlignByEquals.Unaligned00           ))>]
[<TestCase(AlignByEquals.Unaligned01           , TestName = "{m}: " + (nameof AlignByEquals           ) + "." + (nameof AlignByEquals.Unaligned01           ))>]
[<TestCase(AlignByEquals.Unaligned02           , TestName = "{m}: " + (nameof AlignByEquals           ) + "." + (nameof AlignByEquals.Unaligned02           ))>]
[<TestCase(AlignByEquals.Unaligned03           , TestName = "{m}: " + (nameof AlignByEquals           ) + "." + (nameof AlignByEquals.Unaligned03           ))>]
[<TestCase(AlignByEquals.Unaligned04           , TestName = "{m}: " + (nameof AlignByEquals           ) + "." + (nameof AlignByEquals.Unaligned04           ))>]
[<TestCase(AlignByEquals.Unaligned05           , TestName = "{m}: " + (nameof AlignByEquals           ) + "." + (nameof AlignByEquals.Unaligned05           ))>]
[<TestCase(AlignByOther.Other_U_00             , TestName = "{m}: " + (nameof AlignByOther            ) + "." + (nameof AlignByOther.Other_U_00             ))>]
[<TestCase(ComplexAlignmentRequired.Unaligned05, TestName = "{m}: " + (nameof ComplexAlignmentRequired) + "." + (nameof ComplexAlignmentRequired.Unaligned05))>]
[<TestCase(ComplexAlignmentRequired.Unaligned06, TestName = "{m}: " + (nameof ComplexAlignmentRequired) + "." + (nameof ComplexAlignmentRequired.Unaligned06))>]
[<TestCase(ComplexAlignmentRequired.Unaligned07, TestName = "{m}: " + (nameof ComplexAlignmentRequired) + "." + (nameof ComplexAlignmentRequired.Unaligned07))>]
[<TestCase(ComplexAlignmentRequired.Unaligned08, TestName = "{m}: " + (nameof ComplexAlignmentRequired) + "." + (nameof ComplexAlignmentRequired.Unaligned08))>]
[<TestCase(ComplexAlignmentRequired.Unaligned09, TestName = "{m}: " + (nameof ComplexAlignmentRequired) + "." + (nameof ComplexAlignmentRequired.Unaligned09))>]
[<TestCase(ComplexAlignmentRequired.Unaligned10, TestName = "{m}: " + (nameof ComplexAlignmentRequired) + "." + (nameof ComplexAlignmentRequired.Unaligned10))>]
[<TestCase(ComplexAlignmentRequired.Unaligned11, TestName = "{m}: " + (nameof ComplexAlignmentRequired) + "." + (nameof ComplexAlignmentRequired.Unaligned11))>]
[<TestCase(ComplexAlignmentRequired.Unaligned12, TestName = "{m}: " + (nameof ComplexAlignmentRequired) + "." + (nameof ComplexAlignmentRequired.Unaligned12))>]
[<TestCase(ComplexAlignmentRequired.Unaligned13, TestName = "{m}: " + (nameof ComplexAlignmentRequired) + "." + (nameof ComplexAlignmentRequired.Unaligned13))>]
[<TestCase(ComplexAlignmentRequired.Unaligned14, TestName = "{m}: " + (nameof ComplexAlignmentRequired) + "." + (nameof ComplexAlignmentRequired.Unaligned14))>]
[<TestCase(NoAlignmentRequired.Empty           , TestName = "{m}: " + (nameof NoAlignmentRequired     ) + "." + (nameof NoAlignmentRequired.Empty           ))>]
[<TestCase(NoAlignmentRequired.OneLine         , TestName = "{m}: " + (nameof NoAlignmentRequired     ) + "." + (nameof NoAlignmentRequired.OneLine         ))>]
[<TestCase(NoAlignmentRequired.WhiteSpaceOnly  , TestName = "{m}: " + (nameof NoAlignmentRequired     ) + "." + (nameof NoAlignmentRequired.WhiteSpaceOnly  ))>]
let ``Line.ofString >> Line.toString roundtrips correctly`` (x : string) =
    x.Split('\n')
    |> Array.iter (fun x ->
        x
        //|> (fun x -> printfn $"{x}"; x)
        |> Line.ofString
        //|> (fun x -> printfn $"{x}"; x)
        |> Line.toString
        |> shouldEqual x
    )

module RealignToFirstLine =
    let [<Literal>] Temp = """
        type Abc = { Def : G  ; Hijklm : Nop } with member this.Qrstu () = $"{this.Def} ({this.Hijklm})"
        type Ab  = { D   : Efg; Hij    : Klm } with member this.Nopq     = 8
"""

    let [<Literal>] Unaligned00 = """        type Abc = { Def : G; Hijklm : Nop } with member this.Qrstu () = $"{this.Def} ({this.Hijklm})"
        // A comment which shouldn't be realigned
        type Ab     = { D : Efg; Hij : Klm } with   member this.Nopq = 8
"""

    let [<Literal>] Realigned00 = """        type Abc = { Def : G  ; Hijklm : Nop } with member this.Qrstu () = $"{this.Def} ({this.Hijklm})"
        // A comment which shouldn't be realigned
        type Ab  = { D   : Efg; Hij    : Klm } with member this.Nopq     = 8
"""

[<TestCase(RealignToFirstLine.Unaligned00, RealignToFirstLine.Realigned00, TestName = "{m}: " + (nameof RealignToFirstLine) + "." + (nameof RealignToFirstLine.Realigned00))>]
let ``Alignment.realignToFirstLine, always removes excess whitespace and aligns by all required token kinds`` (before : string) (after : string) =
    let actual =
        before.Split('\n')
        |> Alignment.realignToFirstLineExtended
        |> String.concat "\n"

    //dump before actual after

    actual |> shouldEqual after
