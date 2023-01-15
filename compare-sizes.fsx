#r "nuget: Funalysis"

open Funalysis

open System.IO
open Deedle
open Plotly.NET

Plotly.loadMyDefaults ()

let data =
    [| 16; 32; 64; 128 |]
    |> Array.map (fun size ->
        size,
        Frame.ReadCsv(
            $"./src/results/Ising-q2-L{size}-N10000000-1973.csv",
            schema = "beta,E,C,M,X"
        ))

let movingAverage (sequence: float seq) =
    sequence
    |> Seq.windowed 5
    |> Seq.map (Seq.average)

let temperature =
    (snd data.[0]).["beta"].Values
    |> Seq.map (fun beta -> 1. / beta)

data
|> Array.map (fun (size, frame) ->
    Chart.Line(
        temperature,
        frame.["C"].Values,
        ShowMarkers = true,
        Name = $"{size}x{size}"
    ))
|> Chart.combine
|> Chart.withMyLayout
|> Chart.addXAxis @"$\LARGE T\ \text{[a.u.]}$"
|> Chart.addYAxis @"$\LARGE C/N\ \text{[a.u.]}$"
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
|> Chart.saveHtml (Path.Combine(__SOURCE_DIRECTORY__, "compare-sizes"))
