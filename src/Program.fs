module Program

open Domain

open System.IO
open FSharp.Stats
open FSharp.Data
open Argu

type Arguments =
    | [<MainCommand; Mandatory; ExactlyOnce; CliPosition(CliPosition.First)>] Model of
        Model
    | [<Mandatory; ExactlyOnce; EqualsAssignment; AltCommandLine("-L")>] LatticeSize of
        int64
    | [<Mandatory; ExactlyOnce; EqualsAssignment; AltCommandLine("-N")>] Sweeps of
        int
    | [<Mandatory; ExactlyOnce>] Beta of float * float
    | [<EqualsAssignment; AltCommandLine("-S")>] Samples of float
    | [<EqualsAssignment; AltCommandLine("-q")>] NumberOfStates of int
    | [<EqualsAssignment>] Seed of int

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Model _ -> "choose model to simulate: Ising or Potts"
            | Sweeps _ -> "number of Monte Carlo sweeps"
            | Samples _ -> "number of data points to generate (default: 50)"
            | Beta _ -> "value of beta = 1 / k_B T"
            | LatticeSize _ -> "size of the lattice (LxL)"
            | Seed _ -> "seed of random number generator (default: 1973)"
            | NumberOfStates _ -> "number of states q (default: 2)"

type Table =
    CsvProvider<Schema="float,float,float,float?,float?", HasHeaders=false>

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
