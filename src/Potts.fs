module Potts

open Domain
open Helpers
open Tensor
open FSharp.Stats

let inline private kronecker ((s_i, s_j): int * int) =
    if s_i = s_j then 1 else 0

let initLattice parameters =
    HostTensor.randomInt
        parameters.Rng
        (1, parameters.NumOfStates + 1)
        [ parameters.LatticeSize; parameters.LatticeSize ]

let inline countInteractions (i, j) spin (lattice: Tensor<int>) =
    let L = lattice.Shape[0]

    kronecker (spin, lattice.[[ (i - 1L) %/ L; j ]])
    + kronecker (spin, lattice.[[ (i + 1L) %/ L; j ]])
    + kronecker (spin, lattice.[[ i; (j - 1L) %/ L ]])
    + kronecker (spin, lattice.[[ i; (j + 1L) %/ L ]])

let totalEnergy (lattice: Tensor<int>) =
    - float(lattice
            |> HostTensor.mapi (fun index spin ->
                lattice
                |> countInteractions (index[0], index[1]) spin)
            |> Tensor.sum)

let private update parameters (P: float array) (lattice: Tensor<int>) energy =
    (*
        Update function that mutates the supplied lattice in-place.
    *)

    let i, j =
        parameters.Rng
        |> Lattice.randomIndex parameters.LatticeSize

    let oldSpin = lattice.[[ i; j ]]

    let oldSpinEnergy =
        lattice
        |> countInteractions (i, j) oldSpin

    let newSpin =
        parameters.States
        |> Array.filter ((<>) oldSpin)
        |> Array.item (parameters.Rng.Next(0, parameters.NumOfStates - 1))

    let newSpinEnergy =
        lattice
        |> countInteractions (i, j) newSpin

    let dE = oldSpinEnergy - newSpinEnergy

    if
        dE < 0
        || parameters.Rng.NextDouble() < P.[dE + 4]
    then
        lattice.[[ i; j ]] <- newSpin

        energy + float dE
    else
        energy

let simulate (parameters: Parameters) lattice =
    let probabilities =
        Array.init 9 (fun i -> exp (- float(i - 4) * parameters.Beta))

    let mutable energy = totalEnergy lattice

    let steps =
        [|
            for _ in 1 .. parameters.Sweeps do
                yield energy

                let newEnergy =
                    update parameters probabilities lattice energy

                do energy <- newEnergy
        |]

    let avgE =
        (steps |> Array.average)
        / float lattice.NElems

    let C =
        (steps |> Seq.stDev)
        * parameters.Beta ** 2.
        / float lattice.NElems


    {
        Beta = parameters.Beta
        AvgE = avgE
        C = C
        AvgM = None
        X = None
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
