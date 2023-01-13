module Potts

open Domain
open Helpers
open Tensor

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
    -(lattice
      |> HostTensor.mapi (fun index spin ->
          lattice
          |> countInteractions (index[0], index[1]) spin)
      |> Tensor.sum)

let private update
    parameters
    (P: float array)
    (lattice: Tensor<int>)
    energy
    magnetization
    =
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
    let dM = newSpin - oldSpin

    if
        dE < 0
        || parameters.Rng.NextDouble() < P.[dE + 4]
    then
        lattice.[[ i; j ]] <- newSpin

        energy + dE, magnetization + dM
    else
        energy, magnetization

let simulate parameters lattice =
    let probabilities =
        Array.init 9 (fun i -> exp (- float(i - 4) * parameters.Beta))

    let rec loop energy magnetization =
        seq {
            yield energy, magnetization

            let newEnergy, newMagnetization =
                update parameters probabilities lattice energy magnetization

            yield! loop newEnergy newMagnetization
        }

    loop (totalEnergy lattice) (lattice |> Tensor.sum)
