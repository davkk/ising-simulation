module Lattice

open Tensor

type Parameters =
    {
        Rng: System.Random
        LatticeSize: int
        NumOfStates: int
    }

    member this.States =
        if this.NumOfStates > 1 then
            [| 1 .. this.NumOfStates |]
        else
            raise (System.Exception "q must be greater than 1")

let inline private (%/) a b = (a + b) % b

let inline private kronecker ((s_i, s_j): int * int) =
    if s_i = s_j then 1 else 0

let init par =
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

let update par (P: float array) (lattice: Tensor<int>) =
    let i =
        par.Rng.Next(0, par.LatticeSize)
        |> int64

    let j =
        par.Rng.Next(0, par.LatticeSize)
        |> int64

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
        dE
    else
        0

// TODO return lattice, energy change for future use in plot, mag
