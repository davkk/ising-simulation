module Ising

open Domain
open Helpers
open Tensor

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

let private update parameters (P: float array) (lattice: Tensor<int>) energy =
    (*
        Update function that mutates the supplied lattice in-place.
    *)

    let i, j =
        parameters.Rng
        |> Lattice.randomIndex parameters.LatticeSize

    let spin = lattice.[[ i; j ]]

    let dE =
        2 * spin * (lattice |> spinSum (i, j))

    if
        dE < 0
        || parameters.Rng.NextDouble() < P.[dE / 4 + 2]
    then
        lattice.[[ i; j ]] <- -spin

        energy + dE
    else
        energy

let simulate parameters lattice =
    let probabilities =
        [| for dE in -8. .. 4. .. 8. -> exp (-parameters.Beta * dE) |]

    let rec loop i energy =
        seq {
            yield energy

            let newEnergy =
                energy
                |> update parameters probabilities lattice

            yield! loop (i + 1L) newEnergy
        }

    loop 1L (totalEnergy lattice)
