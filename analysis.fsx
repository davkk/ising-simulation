#r "nuget: Funalysis, 1.0.1"

open Funalysis

open System.IO
open FSharp.Stats
open Deedle
open Plotly.NET

let data =
    Frame.ReadCsv(
        "./src/results/Ising-q2-L256-N3000000-1973.csv",
        hasHeaders = false,
        schema = "Beta,E,C,M,X"
    )

Plotly.loadMyDefaults ()

let temperature =
    data.["Beta"]
    |> Series.mapValues (fun beta -> 1. / beta)
    |> Series.values

Seq.zip temperature data.["E"].Values
|> Seq.windowed 5
|> Seq.map (fun window -> fst window[0], window |> Array.averageBy snd)
|> Chart.Line
|> Chart.withMyLayout
|> Chart.saveHtml (Path.Combine(__SOURCE_DIRECTORY__, "energy(beta)"))

