module Ising

open Domain
open Helpers
open Tensor
open FSharp.Stats

let initLattice parameters =
    HostTensor.init
        [ parameters.LatticeSize; parameters.LatticeSize ]
        (fun _ -> if parameters.Rng.NextDouble() < 0.5 then -1 else 1)

let inline spinSum (i, j) (lattice: Tensor<int>) =
    let L = lattice.Shape[0]

    lattice.[[ (i - 1L) %/ L; j ]]
    + lattice.[[ (i + 1L) %/ L; j ]]
    + lattice.[[ i; (j - 1L) %/ L ]]
    + lattice.[[ i; (j + 1L) %/ L ]]

let totalEnergy (lattice: Tensor<int>) =
    -(lattice
      |> HostTensor.mapi (fun index spin ->
          spin * lattice
          |> spinSum (index[0], index[1]))
      |> Tensor.sum)
    / 2

let private update
    parameters
    (P: float array)
    (lattice: Tensor<int>)
    (energy: float)
    magnetization
    =
    (*
        Update function that mutates the supplied lattice in-place.
    *)

    let i, j =
        parameters.Rng
        |> Lattice.randomIndex parameters.LatticeSize

    let spin = lattice.[[ i; j ]]

    let dE =
        2 * spin * (lattice |> spinSum (i, j))

    let dM = -2 * spin

    if
        dE < 0
        || parameters.Rng.NextDouble() < P.[dE / 4 + 2]
    then
        lattice.[[ i; j ]] <- -spin

        energy + float dE, magnetization + float dM
    else
        energy, magnetization

let simulate (parameters: Parameters) lattice =
    let probabilities =
        [| for dE in -8. .. 4. .. 8. -> exp (-parameters.Beta * dE) |]

    let mutable energy =
        totalEnergy lattice |> float

    let mutable magnetization =
        lattice |> Tensor.sum |> float

    let steps =
        [|
            for _ in 1 .. parameters.Sweeps do
                yield energy, magnetization

                let newEnergy, newMagnetization =
                    update parameters probabilities lattice energy magnetization

                energy <- newEnergy
                magnetization <- newMagnetization
        |]

    let avgE =
        (steps |> Array.averageBy fst)
        / float lattice.NElems

    let C =
        (steps |> Array.map fst |> Seq.stDev)
        * parameters.Beta ** 2.
        / float lattice.NElems

    let avgM =
        (steps |> Array.averageBy (snd >> abs))
        / float lattice.NElems

    let X =
        (steps
         |> Array.map (snd >> abs)
         |> Seq.stDev)
        * parameters.Beta
        / float lattice.NElems

    {
        Beta = parameters.Beta
        AvgE = avgE
        C = C
        AvgM = Some avgM
        X = Some X
    }

// let rec loop energy magnetization =
//     seq {
//         yield energy, magnetization
//
//         let newEnergy, newMagnetization =
//             update parameters probabilities lattice energy magnetization
//
//         yield! loop newEnergy newMagnetization
//     }
//
// loop (totalEnergy lattice) (lattice |> Tensor.sum)
