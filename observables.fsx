#r "nuget: Funalysis"

open Funalysis

open System.IO
open Deedle
open Plotly.NET

Plotly.loadMyDefaults ()

let data =
    Frame.ReadCsv(
        "./src/results/Ising-q2-L256-N3000000-1980.csv",
        schema = "beta,E,C,M,X"
    )

let temperature =
    data.["beta"].Values
    |> Seq.map (fun beta -> 1. / beta)

let movingAverage (sequence: (float * float) seq) =
    sequence
    |> Seq.windowed 5
    |> Seq.map (fun window -> fst window.[0], window |> Seq.averageBy snd)

[
    Seq.zip temperature data.["E"].Values
    Seq.zip temperature data.["C"].Values
    Seq.zip temperature data.["M"].Values
    Seq.zip temperature data.["X"].Values
]
|> Seq.zip
    [
        @"$\LARGE E / N\ \text{[a.u.]}$"
        @"$\LARGE C_V / N\ \text{[a.u.]}$"
        @"$\LARGE |M| / N\ \text{[a.u.]}$"
        @"$\LARGE \chi / N\ \text{[a.u.]}$"
    ]
|> Seq.map (fun (name, data) ->
    Chart.Line(data |> movingAverage, ShowMarkers = true)
    |> Chart.addYAxis name)
|> Chart.Grid(4, 1, Pattern = StyleParam.LayoutGridPattern.Coupled)
|> Chart.withMyLayout
|> Chart.withLegend false
|> Chart.withShape (
    LayoutObjects.Shape.init (
        Yref = "paper",
        X0 = 2.27,
        X1 = 2.27,
        Y0 = 0,
        Y1 = 1,
        Line =
            Line.init (
                Color = Color.fromKeyword Gray,
                Dash = StyleParam.DrawingStyle.Dash
            )
    )
)
|> Chart.withAnnotation (
    LayoutObjects.Annotation.init (
        YRef = "paper",
        X = 2.27,
        Y = 0,
        AX = 20,
        AY = 40,
        XAnchor = StyleParam.XAnchorPosition.Left,
        Text = "2.27"
    )
)
|> Chart.addXAxis @"$\LARGE T\ \text{[a.u.]}$"
|> Chart.withSize (Height = 1400)
|> Chart.saveHtml (Path.Combine(__SOURCE_DIRECTORY__, "observables.html"))
