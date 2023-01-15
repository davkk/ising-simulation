#r "nuget: Funalysis"

open Funalysis

open System.IO
open Deedle
open Plotly.NET

Plotly.loadMyDefaults ()

let ising =
    Frame.ReadCsv(
        "./src/results/Ising-q2-L128-N1000000-1973.csv",
        schema = "beta,E,C,M,X"
    )

let potts =
    Frame.ReadCsv(
        "./src/results/Potts-q2-L128-N1000000-1973.csv",
        schema = "beta,E,C,,"
    )

let reciprocal sequence =
    sequence
    |> Seq.map (fun beta -> 1. / beta)

let movingAverage (sequence: (float) seq) =
    sequence
    |> Seq.windowed 5
    |> Seq.map (Seq.average)


[
    Chart.Line(
        Seq.zip
            (reciprocal ising.["beta"].Values)
            (ising.["C"].Values |> movingAverage),
        ShowMarkers = true,
        Name = "Model Isinga"
    )
    Chart.Line(
        Seq.zip
            (reciprocal potts.["beta"].Values)
            (potts.["C"].Values |> movingAverage),
        ShowMarkers = true,
        Name = "Model Pottsa"
    )
]
|> Chart.combine
|> Chart.withMyLayout
|> Chart.withShapes[ 
    LayoutObjects.Shape.init (
        Yref = "paper",
        X0 = 1.13,
        X1 = 1.13,
        Y0 = 0,
        Y1 = 1,
        Line =
            Line.init (
                Color = Color.fromKeyword Gray,
                Dash = StyleParam.DrawingStyle.Dash
            )
    )
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
]
|> Chart.withAnnotations
    [
        LayoutObjects.Annotation.init (
            YRef = "paper",
            X = 1.13,
            Y = 0,
            AX = 20,
            AY = 40,
            XAnchor = StyleParam.XAnchorPosition.Left,
            Text = "1.13"
        )
        LayoutObjects.Annotation.init (
            YRef = "paper",
            X = 2.27,
            Y = 0,
            AX = 20,
            AY = 40,
            XAnchor = StyleParam.XAnchorPosition.Left,
            Text = "2.27"
        )
    ]
|> Chart.addXAxis @"$\LARGE T\ \text{[a.u.]}$"
|> Chart.addYAxis @"$\LARGE C_V/N\ \text{[a.u.]}$"
|> Chart.saveHtml (Path.Combine(__SOURCE_DIRECTORY__, "compare-models"))
