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

        let steps =
            lattice
            |> simulate parameters
            |> Seq.take parameters.Sweeps
            |> Seq.map float
            |> Array.ofSeq

        printfn $"[beta=%.2f{parameters.Beta}] Simulation done!"

        let cutoff =
            steps.Length
            - (float parameters.Sweeps * 0.1 |> int)

        parameters.Beta,
        steps.[cutoff..] |> Array.average,
        steps.[cutoff..]
        |> Array.averageBy (fun E -> E ** 2.)
