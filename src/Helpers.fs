module Helpers

open Domain

let inline (%/) a b = (a + b) % b

module Lattice =
    let randomIndex max (rng: System.Random) =
        rng.NextInt64(0, max), rng.NextInt64(0, max)

module Simulation =
    let run parameters initLattice simulate =
        printfn ""
        printfn $"[beta=%.2f{parameters.Beta}] Initializing the lattice..."
        let lattice = initLattice parameters

        printfn $"[beta=%.2f{parameters.Beta}] Running the simulation..."

        let steps: Stats array =
            lattice |> simulate parameters

        printfn $"[beta=%.2f{parameters.Beta}] Simulation done!"

       // steps.[steps.Length - 100 ..]
