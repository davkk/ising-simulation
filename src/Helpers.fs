module Helpers

let inline (%/) a b = (a + b) % b

module Lattice =
    let randomIndex max (rng: System.Random) =
        rng.Next(0, max) |> int64, rng.Next(0, max) |> int64
