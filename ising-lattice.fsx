#r "nuget: Funalysis"
#r "nuget: Tensor"

#load "./src/Domain.fs"
#load "./src/Helpers.fs"
#load "./src/Ising.fs"

open Funalysis

open System.IO
open Plotly.NET
open Tensor

open Domain
open Ising

Plotly.loadMyDefaults ()

let parameters: Parameters =
    {
        Rng = System.Random(2001)
        Sweeps = 10000000
        Beta = 1.8
        LatticeSize = 256
        NumOfStates = 2
    }

let initialLattice =
    Ising.initLattice parameters

let lattice = initialLattice |> Tensor.copy

let stats, steps =
    Ising.simulate parameters lattice

[ initialLattice; lattice ]
|> Seq.map (fun lat ->
    Chart.Heatmap(
        lat |> HostTensor.toList2D,
        ColorScale = StyleParam.Colorscale.Hot
    ))
|> Chart.Grid(1, 2)
|> Chart.withMyLayout
|> Chart.withSize (Height = 600)
|> Chart.saveHtml (
    Path.Combine(__SOURCE_DIRECTORY__, "lattice-evolution-Ising-256x256")
)

[
    steps
    |> Array.indexed
    |> Array.map (fun (i, (E, _)) -> i, E)
    |> Array.filter (fun (i, _) -> i % (parameters.Sweeps / 1000) = 0)
    |> Chart.Point
    |> Chart.addXAxis "Liczba kroków MC"
    |> Chart.addYAxis "Energia [a.u.]"
    steps
    |> Array.indexed
    |> Array.map (fun (i, (_, M)) -> i, M)
    |> Array.filter (fun (i, _) -> i % (parameters.Sweeps / 1000) = 0)
    |> Chart.Point
    |> Chart.addXAxis "Liczba kroków MC"
    |> Chart.addYAxis "Magnetyzacja [a.u.]"
]
|> Chart.Grid(2, 1, Pattern = StyleParam.LayoutGridPattern.Coupled)
|> Chart.withMyLayout
|> Chart.withLegend false
|> Chart.saveHtml (Path.Combine(__SOURCE_DIRECTORY__, "E(t)+M(t)"))

// steps
// |> Array.indexed
// |> Array.map (fun (i, (_, M)) -> i, M)
// |> Array.filter (fun (i, _) -> i % (parameters.Sweeps / 1000) = 0)
// |> Chart.Point
// |> Chart.withMyLayout
// |> Chart.withLegend false
// |> Chart.addXAxis "Liczba kroków MC"
// |> Chart.addYAxis "Magnetyzacja [a.u.]"
// |> Chart.saveHtml (Path.Combine(__SOURCE_DIRECTORY__, "M(t)"))
