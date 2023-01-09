module Potts

open Helpers

open Tensor

type Parameters =
    {
        Rng: System.Random
        LatticeSize: int
        NumOfStates: int
        Beta: float
    }

    member this.States =
        if this.NumOfStates > 1 then
            [| 1 .. this.NumOfStates |]
        else
            failwith "q must be greater than 1"

let inline private kronecker ((s_i, s_j): int * int) =
    if s_i = s_j then 1 else 0

let initLattice par =
    HostTensor.randomInt
        par.Rng
        (1, par.NumOfStates + 1)
        [ par.LatticeSize; par.LatticeSize ]

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

let private update par (P: float array) (lattice: Tensor<int>) energy =
    (*
        Update function that mutates the supplied lattice in-place.
    *)

    let i, j =
        par.Rng
        |> Lattice.randomIndex par.LatticeSize

    let oldSpin = lattice.[[ i; j ]]

    let oldSpinEnergy =
        lattice
        |> countInteractions (i, j) oldSpin

    let newSpin =
        par.States
        |> Array.filter ((<>) oldSpin)
        |> Array.item (par.Rng.Next(0, par.NumOfStates - 1))

    let newSpinEnergy =
        lattice
        |> countInteractions (i, j) newSpin

    let dE = oldSpinEnergy - newSpinEnergy

    if
        dE < 0
        || par.Rng.NextDouble() < P.[dE + 4]
    then
        lattice.[[ i; j ]] <- newSpin

        energy + dE
    else
        energy

let simulate parameters lattice =
    let probabilities =
        Array.init 9 (fun i -> exp (- float(i - 4) * parameters.Beta))

    let rec loop i energy =
        seq {
            yield energy

            let newEnergy =
                energy
                |> update parameters probabilities lattice

            yield! loop (i + 1L) newEnergy
        }

    loop 1L (totalEnergy lattice)
