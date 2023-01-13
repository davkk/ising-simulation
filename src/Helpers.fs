module Helpers

open Domain
open Tensor

let inline (%/) a b = (a + b) % b

module Lattice =
    let randomIndex max (rng: System.Random) =
        rng.NextInt64(0, max), rng.NextInt64(0, max)

module Simulation =
    let run
        parameters
        initLattice
        (simulate: Parameters -> Tensor<int> -> (int * int) seq)
        =
        printfn ""
        printfn $"[beta=%.2f{parameters.Beta}] Initializing the lattice..."
        let lattice = initLattice parameters

        printfn $"[beta=%.2f{parameters.Beta}] Running the simulation..."

        let steps =
            lattice
            |> simulate parameters
            |> Seq.take parameters.Sweeps
            |> Array.ofSeq

        printfn $"[beta=%.2f{parameters.Beta}] Simulation done!"

        parameters.Beta, steps
