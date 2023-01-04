module Program

open System.IO

open Lattice
open FSharp.Stats
open Plotly.NET
open Plotly.NET.LayoutObjects
open Plotly.NET.TraceObjects

// TODO refactor with Row: https://github.com/matthewcrews/FSharpPerformance/tree/main/Row
type Step =
    {
        Beta: float
        Energy: float
        Magnetization: float
        Lattice: Lattice
    }

let update (rng: System.Random) (dE: int array) (P: float array) step =
    let lattice = step.Lattice

    // choose random spin
    let i = rng.Next(0, lattice.Size)
    let j = rng.Next(0, lattice.Size)

    let oldSpin = lattice.Spins[i, j]

    let newSpin =
        lattice.States
        |> Seq.filter ((<>) lattice.Spins[i, j])
        |> Seq.item (rng.Next(0, lattice.Q - 1))

    let neighborCount =
        lattice |> Lattice.neighborCount i j

    let k =
        int (
            neighborCount.[oldSpin - 1]
            - neighborCount.[newSpin - 1]
        )
        + 4

    if dE.[k] < 0 || rng.NextDouble() < P.[k] then
        let newLattice =
            let copy = lattice.Spins |> Array2D.copy

            copy[i, j] <- newSpin

            { lattice with Spins = copy }

        { step with
            Lattice = newLattice
            Energy = Lattice.totalEnergy lattice
            Magnetization = 0.
        }
    else
        step

let simulate rng beta lattice =
    let dE = [| -4 .. 4 |]

    let P =
        [| for i in dE -> exp (-float i * beta) |]

    let rec simulate' step =
        seq {
            yield step
            yield! step |> update rng dE P |> simulate'
        }

    simulate'
        {
            Lattice = lattice
            Beta = beta
            Energy = lattice |> Lattice.totalEnergy
            Magnetization = lattice |> Lattice.magnetization
        }

[<EntryPoint>]
let main _ = // TODO add args params
    let seed = 1973
    let random = System.Random(seed)

    let N_sweeps = 100000
    let L = 128
    let q = 2
    let beta = 1.4

    let lattice = Lattice.initRandom L q random

    let steps =
        simulate random beta lattice
        |> Seq.take N_sweeps
        |> Array.ofSeq

    [ for x in 0 .. N_sweeps - 1 -> x, steps.[x].Energy ]
    |> Chart.Point
    |> Chart.saveHtml (Path.Combine(__SOURCE_DIRECTORY__, "Energy vs Time"))

    [ (steps |> Seq.head); (steps |> Seq.last) ]
    |> Seq.map (fun step ->
        step.Lattice.Spins
        |> Seq.cast<int>
        |> Seq.splitInto L
        |> Chart.Heatmap)
    |> Chart.Grid (1, 2)
    |> Chart.withSize (1600, 850)
    |> Chart.saveHtml (Path.Combine(__SOURCE_DIRECTORY__, "heatmap"))


    0
