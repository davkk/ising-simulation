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

type Table =
    CsvProvider<Schema="Beta (float),Energy (float),Magnetization (float)", HasHeaders=false>

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
            Beta = 0.0
        }

    let simulations =
        [|
            for beta in betaMin .. (betaMax - betaMin) / samples .. betaMax do
                yield
                    match model with
                    | Ising ->
                        Simulation.run
                            { parameters with
                                NumOfStates = 2
                                Beta = beta
                            }
                            Ising.initLattice
                            Ising.simulate
                    | Potts ->
                        Simulation.run
                            { parameters with Beta = beta }
                            Potts.initLattice
                            Potts.simulate
        |]


    let buildRow (beta, energy, magnetization) =
        Table.Row(beta, energy, magnetization)

    let table =
        new Table(
            simulations
            |> Seq.map (fun (beta, steps) ->
                steps.[steps.Length - 100 ..]
                |> Seq.map (fun (energy, magnetization) ->
                    buildRow (beta, energy, magnetization)))
            |> Seq.collect id
        )

    let resultsPath =
        Path.Combine(__SOURCE_DIRECTORY__, "results")

    Directory.CreateDirectory(resultsPath)
    |> ignore

    table.Save(
        Path.Combine(
            resultsPath,
            $"{model}-q{parameters.NumOfStates}-L{parameters.LatticeSize}-N{parameters.Sweeps}-{seed}.csv"
        )
    )

    0
