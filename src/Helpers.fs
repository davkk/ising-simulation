module Helpers

let inline (%/) a b = (a + b) % b

type Parameters =
    {
        Rng: System.Random
        LatticeSize: int
        NumOfStates: int
        Beta: float
    }

    member this.States =
        if this.NumOfStates > 1 then
            [| 1 .. this.NumOfStates |]
        else
            failwith "q must be greater than 1"

module Lattice =
    let randomIndex max (rng: System.Random) =
        rng.NextInt64(0, max), rng.NextInt64(0, max)
