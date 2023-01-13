#r "nuget: Funalysis, 1.0.1"

open Funalysis

open System.IO
open FSharp.Stats
open Deedle
open Plotly.NET

let data =
    Frame.ReadCsv(
        "./src/results/Ising-q2-L32-N10000000-1973.csv",
        hasHeaders = false,
        schema = "beta,energy,magnetization"
    )

Plotly.loadMyDefaults ()

let temperature =
    data.["beta"]
    |> Series.mapValues (fun beta -> 1. / beta)
    |> Series.values

Seq.zip3 temperature data.["energy"].Values data.["magnetization"].Values
// |> Seq.windowed 10
// |> Seq.map (fun window ->
//     window
//     |> Seq.fold
//         (fun (T1, E1, M1) (_, E2, M2) -> T1, E1 + E2, M1 + M2)
//         (window.[0]))
|> Seq.groupBy (fun (T, _, _) -> T)
|> Seq.map (fun (T, points) ->
    let avgE =
        points
        |> Seq.averageBy (fun (_, E, _) -> E)

    let varE =
        points
        |> Seq.map (fun (_, E, _) -> E)
        |> Seq.stDev

    let avgM =
        points
        |> Seq.averageBy (fun (_, _, M) -> M)

    let varM =
        points
        |> Seq.map (fun (_, _, M) -> M)
        |> Seq.stDev

    T, avgE, varE, avgM, varM)
|> Seq.map (fun (T, E, varE, M, varM) -> T, varM / T ** 2.)
|> Chart.Line
|> Chart.withMyLayout
// |> Chart.withXAxisStyle (MinMax = (0, 4))
|> Chart.saveHtml (Path.Combine(__SOURCE_DIRECTORY__, "energy(beta)"))

// TODO moving averageBy

#r "nuget: Argu"
#r "nuget: Tensor"
#load "src/Domain.fs"
#load "src/Helpers.fs"
#load "src/Ising.fs"

open Tensor
open Domain
open Ising

let parameters =
    {
        Rng = System.Random()
        LatticeSize = 64
        Beta = 0.5
        NumOfStates = 2
        Sweeps = 10_000_000
    }

let lattice = Ising.initLattice parameters

lattice
|> Ising.simulate parameters
|> Seq.take parameters.Sweeps
|> Seq.indexed
|> Seq.map (fun (i, (E, M)) -> i, E)
|> Seq.filter (fun (i, _) -> i % 1000 = 0)
|> Chart.Point
|> Chart.saveHtml (Path.Combine(__SOURCE_DIRECTORY__, "E(t)"))

lattice
|> HostTensor.toList2D
|> Chart.Heatmap
|> Chart.saveHtml (Path.Combine(__SOURCE_DIRECTORY__, "heatmap"))
