module Domain

type Parameters =
    {
        Rng: System.Random
        Sweeps: int
        LatticeSize: int64
        NumOfStates: int
        Beta: float
    }

    member this.States =
        if this.NumOfStates > 1 then
            [| 1 .. this.NumOfStates |]
        else
            failwith "q must be greater than 1"

type Model =
    | Ising
    | Potts

type Stats =
    {
        Beta: float
        AvgE: float
        C: float
        AvgM: float option
        X: float option
    }

