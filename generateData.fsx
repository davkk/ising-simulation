#r "nuget: Funalysis, 1.0.1"

open Funalysis

open System.IO
open FSharp.Stats
open Deedle
open Plotly.NET

let data =
    Frame.ReadCsv(
        "./src/results/test.csv",
        hasHeaders = false,
        schema = "beta,energy,energy2"
    )

Plotly.loadMyDefaults ()

let temperature =
    data.["beta"]
    |> Series.mapValues (fun beta -> 1. / beta)
    |> Series.values

Seq.zip3 temperature data.["energy"].Values data.["energy2"].Values
|> Seq.map (fun (T, E, E2) -> T, E)
|> Chart.Line
|> Chart.withMyLayout
// |> Chart.withXAxisStyle (MinMax = (0, 4))
|> Chart.saveHtml (Path.Combine(__SOURCE_DIRECTORY__, "energy(beta)"))

// TODO moving average
