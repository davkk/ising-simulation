module Program

open Domain
open Helpers

open System.IO
open FSharp.Stats
open Argu
open FSharp.Data

// lattice
// |> HostTensor.toList2D
// |> Chart.Heatmap
// |> Chart.saveHtml (Path.Combine(__SOURCE_DIRECTORY__, "heatmap"))
//
// steps
// |> Array.filter (fun (i, _) -> i % (parameters.Sweeps / 1000) = 0)
// |> Chart.Spline
// |> Chart.saveHtml (Path.Combine(__SOURCE_DIRECTORY__, "Energy vs Time"))


[<EntryPoint>]
let main argv =
    let parser =
        ArgumentParser.Create<Arguments>(programName = "ising.exe")

    let args =
        parser.ParseCommandLine(inputs = argv, raiseOnUsage = true)

    let model = args.GetResult Model

    let betaMin, betaMax = args.GetResult Beta

    let samples =
        args.TryGetResult Samples
        |> Option.defaultWith (fun _ -> 50.)

    let seed =
        args.TryGetResult Seed
        |> Option.defaultWith (fun _ -> 1973)

    let parameters: Parameters =
        {
            Rng = System.Random(seed)
            Sweeps = args.GetResult Sweeps
            LatticeSize = args.GetResult LatticeSize
            NumOfStates =
                args.TryGetResult NumberOfStates
                |> Option.defaultWith (fun _ -> 2)
            Beta = betaMin
        }

    let initLattice, simulate =
        match model with
        | Ising -> Ising.initLattice, Ising.simulate
        | Potts -> Potts.initLattice, Potts.simulate

    let lattice = initLattice parameters

    // let the system reach initial equilibrium
    printfn "[*] Relaxation of the initial lattice..."

    lattice
    |> simulate
        { parameters with
            Sweeps = 10_000 * int parameters.LatticeSize
        }
    |> ignore

    let data =
        [|
            for beta in betaMin .. (betaMax - betaMin) / samples .. betaMax do
                printfn ""
                printfn $"[beta=%.2f{beta}] Running the simulation..."

                let result =
                    lattice
                    |> simulate { parameters with Beta = beta }

                printfn $"%A{result}"

                yield result
        |]

    let resultsPath =
        Path.Combine(__SOURCE_DIRECTORY__, "results")

    Directory.CreateDirectory(resultsPath)
    |> ignore

    let fileName model q L N seed = $"{model}-q{q}-L{L}-N{N}-{seed}.csv"

    let table =
        new Table(
            data.[data.Length - 100 ..]
            |> Array.map (fun stats ->
                match model with
                | Ising ->
                    Table.Row(
                        stats.Beta,
                        stats.AvgE,
                        stats.C,
                        System.Nullable(stats.AvgM |> Option.defaultValue 0.0),
                        System.Nullable(stats.X |> Option.defaultValue 0.0)
                    )
                | Potts ->
                    Table.Row(
                        stats.Beta,
                        stats.AvgE,
                        stats.C,
                        System.Nullable(),
                        System.Nullable()
                    ))

        )

    table.Save(
        Path.Combine(
            resultsPath,
            fileName
                model
                parameters.NumOfStates
                parameters.LatticeSize
                parameters.Sweeps
                seed
        )
    )

    0
