module Program

open System.IO

open Tensor

open FSharp.Stats

open Plotly.NET


[<EntryPoint>]
let main _ = // TODO add args params
    let seed = 1973
    let sweeps = 10_000_000
    let beta = 1.4

    let parameters: Potts.Parameters =
        {
            Rng = System.Random(seed)
            LatticeSize = 256
            NumOfStates = 4
            Beta = beta
        }

    printfn "[*] Initializing the lattice..."
    let lattice = Potts.initLattice parameters

    printfn "[*] Running the simulation..."
    let steps =
        lattice
        |> Potts.simulate parameters
        |> Seq.take sweeps
        |> Array.ofSeq
    printfn "[+] Simulation done!"

    lattice
    |> HostTensor.toList2D
    |> Chart.Heatmap
    |> Chart.saveHtml (Path.Combine(__SOURCE_DIRECTORY__, "heatmap"))

    steps
    |> Array.mapi (fun i energy -> i, energy)
    |> Array.filter (fun (i, _) -> i % (sweeps / 1000) = 0)
    |> Chart.Spline
    |> Chart.saveHtml (Path.Combine(__SOURCE_DIRECTORY__, "Energy vs Time"))

    0
