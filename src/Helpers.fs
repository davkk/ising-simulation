namespace Helpers

module Array2D =
    let toArray (arr2d: 'T[,]) = arr2d |> Seq.cast<'T> |> Array.ofSeq


module Math =
    let kronecker (s_i, s_j) =
        match s_i = s_j with
        | true -> 1.
        | false -> 0.
