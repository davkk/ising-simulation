module Program

open System.IO

open Lattice
open Tensor

open FSharp.Stats

open Plotly.NET


[<EntryPoint>]
let main _ = // TODO add args params
    let seed = 1973
    let sweeps = 100_000_000
    let beta = 1.4

    let parameters =
        {
            Rng = System.Random(seed)
            LatticeSize = 512
            NumOfStates = 3
        }

    let probabilities =
        Array.init 9 (fun i -> exp (- float(i - 4) * beta))

    let lattice =
        Lattice.init parameters

    let initialLatticeChart =
        lattice
        |> HostTensor.toList2D
        |> Chart.Heatmap

    let simulate sweeps lattice =
        let mutable energy =
            lattice |> Lattice.totalEnergy

        [|
            for _ in 1..sweeps do
                let dE =
                    lattice
                    |> update parameters probabilities

                energy <- energy + dE
                yield energy
        |]

    let steps = lattice |> simulate sweeps

    [
        initialLatticeChart
        lattice
        |> HostTensor.toList2D
        |> Chart.Heatmap
    ]
    |> Chart.Grid(1, 2)
    |> Chart.withSize (1400, 740)
    |> Chart.saveHtml (Path.Combine(__SOURCE_DIRECTORY__, "heatmap"))

    steps
    |> Array.mapi (fun i energy -> i, energy)
    |> Array.filter (fun (i, _) -> i % (sweeps / 1000) = 0)
    |> Chart.Spline
    |> Chart.saveHtml (Path.Combine(__SOURCE_DIRECTORY__, "Energy vs Time"))

    0
