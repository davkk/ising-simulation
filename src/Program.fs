module Program

open Helpers

open System.IO
open Tensor
open FSharp.Stats
open Plotly.NET
open Argu

type Model =
    | Ising
    | Potts

type Arguments =
    | [<MainCommand; Mandatory; ExactlyOnce; CliPosition(CliPosition.First)>] Model of
        Model
    | [<Mandatory; ExactlyOnce; EqualsAssignment; AltCommandLine("-L")>] LatticeSize of
        int64
    | [<Mandatory; ExactlyOnce; EqualsAssignment; AltCommandLine("-N")>] Sweeps of
        int
    | [<Mandatory; ExactlyOnce; EqualsAssignment>] Beta of float
    | [<EqualsAssignment; AltCommandLine("-q")>] NumberOfStates of int
    | [<ExactlyOnce; EqualsAssignment>] Seed of int


    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Model _ -> "choose model to simulate: Ising or Potts"
            | Sweeps _ -> "number of Monte Carlo sweeps"
            | Beta _ -> "value of beta = 1 / k_B T"
            | LatticeSize _ -> "size of the lattice (LxL)"
            | Seed _ -> "seed of random number generator (default: 1973)"
            | NumberOfStates _ -> "number of states q (default: 2)"

let run parameters initLattice simulate =
    printfn "[*] Initializing the lattice..."
    let lattice = initLattice parameters

    printfn "[*] Running the simulation..."

    let steps =
        lattice
        |> simulate parameters
        |> Seq.take parameters.Sweeps
        |> Seq.indexed
        |> Array.ofSeq

    printfn "[+] Simulation done!"

    lattice
    |> HostTensor.toList2D
    |> Chart.Heatmap
    |> Chart.saveHtml (Path.Combine(__SOURCE_DIRECTORY__, "heatmap"))

    steps
    |> Array.filter (fun (i, _) -> i % (parameters.Sweeps / 1000) = 0)
    |> Chart.Spline
    |> Chart.saveHtml (Path.Combine(__SOURCE_DIRECTORY__, "Energy vs Time"))

[<EntryPoint>]
let main argv =
    let parser =
        ArgumentParser.Create<Arguments>(programName = "ising.exe")

    let args =
        parser.ParseCommandLine(inputs = argv, raiseOnUsage = true)

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
            Beta = args.GetResult Beta
        }

    match args.GetResult Model with
    | Ising ->
        run
            ({ parameters with NumOfStates = 2 })
            Ising.initLattice
            Ising.simulate
    | Potts -> run parameters Potts.initLattice Potts.simulate

    0
