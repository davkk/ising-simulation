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

type Table = CsvProvider<Schema="Beta (float),Energy (float),Energy^2 (float)", HasHeaders=false>

[<EntryPoint>]
let main argv =
    let parser =
        ArgumentParser.Create<Arguments>(programName = "ising.exe")

    let args =
        parser.ParseCommandLine(inputs = argv, raiseOnUsage = true)

    let betaMin, betaMax = args.GetResult Beta

    let samples = args.TryGetResult Samples |> Option.defaultWith (fun _ -> 50.)

    let simulations =
        [|
            for beta in betaMin .. (betaMax - betaMin) / samples .. betaMax do
                let parameters: Parameters =
                    {
                        Rng =
                            System.Random(
                                args.TryGetResult Seed
                                |> Option.defaultWith (fun _ -> 1973)
                            )
                        Sweeps = args.GetResult Sweeps
                        LatticeSize = args.GetResult LatticeSize
                        NumOfStates =
                            args.TryGetResult NumberOfStates
                            |> Option.defaultWith (fun _ -> 2)
                        Beta = beta
                    }

                yield
                    match args.GetResult Model with
                    | Ising ->
                        Simulation.run
                            ({ parameters with NumOfStates = 2 })
                            Ising.initLattice
                            Ising.simulate
                    | Potts ->
                        Simulation.run
                            parameters
                            Potts.initLattice
                            Potts.simulate
        |]


    let buildRow (beta, energy, energy2) = Table.Row(beta, energy, energy2)

    let table = new Table(simulations |> Seq.map buildRow)

    let resultsPath = Path.Combine (__SOURCE_DIRECTORY__, "results")
    Directory.CreateDirectory (resultsPath) |> ignore

    table.Save(Path.Combine (resultsPath, "test.csv"))

    0
