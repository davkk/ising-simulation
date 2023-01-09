namespace Helpers

module Array2D =
    let toArray (arr2d: 'T[,]) = arr2d |> Seq.cast<'T> |> Array.ofSeq


