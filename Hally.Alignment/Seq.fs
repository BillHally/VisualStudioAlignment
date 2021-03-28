module Hally.Alignment.Seq

[<CompiledName("TryFindIndexI")>]
let tryFindIndexi predicate (source:seq<_>) =
    //checkNonNull "source" source
    use ie = source.GetEnumerator()
    let rec loop i =
        if ie.MoveNext() then
            if predicate i ie.Current then
                Some i
            else loop (i + 1)
        else
            None
    loop 0
