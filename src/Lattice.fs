module Lattice

open Helpers
open FSharp.Stats
open Microsoft.FSharp.Data.UnitSystems.SI.UnitNames

exception StateNumberError of string

type Lattice =
    {
        Size: int
        Q: int
        Spins: int[,]
    }

    member this.States =
        if this.Q > 1 then
            [| 1 .. this.Q |]
        else
            raise (StateNumberError("q must be greater than one"))

let initRandom L q (rng: System.Random) =
    {
        Size = L
        Q = q
        Spins = Array2D.init L L (fun _ _ -> rng.Next(1, q + 1))
    }

let initCold L q =
    {
        Size = L
        Q = q
        Spins = Array2D.zeroCreate L L
    }

let inline private (%%) n max = (n + max) % max

let neighborCount i j lattice =
    let neighbors =
        [|
            lattice.Spins[(i - 1) %% lattice.Size, j]
            lattice.Spins[(i + 1) %% lattice.Size, j]
            lattice.Spins[i, (j - 1) %% lattice.Size]
            lattice.Spins[i, (j + 1) %% lattice.Size]
        |]

    let counts = Array.zeroCreate 4

    // TODO time against Array.countBy id
    for i = 0 to 3 do
        counts[neighbors[i] - 1] <- counts[neighbors[i] - 1] + 1

    counts

// let energy i j lattice =
//     let current = lattice.Spins[i, j]
//
//     [|
//         lattice.Spins[bc (i - 1) lattice.Size, j]
//         lattice.Spins[bc (i + 1) lattice.Size, j]
//         lattice.Spins[i, bc (j - 1) lattice.Size]
//         lattice.Spins[i, bc (j + 1) lattice.Size]
//     |]
//     |> Seq.map (fun neighbor -> Math.kronecker (current, neighbor))
//     |> Seq.sum


let totalEnergy lattice =
    let sum =
        lattice.Spins
        |> Array2D.mapi (fun i j spin -> (neighborCount i j lattice).[spin - 1])
        |> Array2D.toArray
        |> Array.sum
        |> float

    let average = 
        sum / float (lattice.Size * lattice.Size)

    // let sum =
    //     lattice.Spins
    //     |> Array2D.mapi (fun x y curr ->
    //         kronecker (curr, lattice.Spins[bc (x - 1) lattice.Size, y])
    //         + kronecker (curr, lattice.Spins[bc (x + 1) lattice.Size, y])
    //         + kronecker (curr, lattice.Spins[x, bc (y - 1) lattice.Size])
    //         + kronecker (curr, lattice.Spins[x, bc (y + 1) lattice.Size]))
    //     |> Seq.cast<float>
    //     |> Seq.sum

    -average / 2.


let magnetization lattice =
    // (lattice.Spins
    //  |> Seq.cast<float>
    //  |> Seq.sum)
    // / float (lattice.Size * lattice.Size)
    0.


// let print (lattice: Matrix<float>) =
//     let min, max = Matrix.colRange lattice
//
//     for i in min .. max do
//         for j in min .. max do
//             Matrix.get lattice i j
//             |> int
//             |> match (j + 1) % (max + 1) with
//                | 0 -> printf "%d\n"
//                | _ -> printf "%d "
