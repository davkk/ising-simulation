module Domain

open Argu
open FSharp.Data

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

// type PottsStats = { Beta: float; AvgE: float; C: float }
//
// type Stats =
//     | IsingStats of IsingStats
//     | PottsStats of PottsStats

type Table =
    CsvProvider<Schema="float,float,float,float?,float?", HasHeaders=false>

type IsingTable =
    CsvProvider<Schema="Beta (float),Average Energy (float),Heat Capacity (float),Magnetization (float),Magnetic Susceptibility (float)", HasHeaders=false>

type PottsTable =
    CsvProvider<Schema="Beta (float),Average Energy (float),Heat Capacity (float)", HasHeaders=false>

type Arguments =
    | [<MainCommand; Mandatory; ExactlyOnce; CliPosition(CliPosition.First)>] Model of
        Model
    | [<Mandatory; ExactlyOnce; EqualsAssignment; AltCommandLine("-L")>] LatticeSize of
        int64
    | [<Mandatory; ExactlyOnce; EqualsAssignment; AltCommandLine("-N")>] Sweeps of
        int
    | [<Mandatory; ExactlyOnce>] Beta of float * float
    | [<EqualsAssignment; AltCommandLine("-S")>] Samples of float
    | [<EqualsAssignment; AltCommandLine("-q")>] NumberOfStates of int
    | [<EqualsAssignment>] Seed of int

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Model _ -> "choose model to simulate: Ising or Potts"
            | Sweeps _ -> "number of Monte Carlo sweeps"
            | Samples _ -> "number of data points to generate (default: 50)"
            | Beta _ -> "value of beta = 1 / k_B T"
            | LatticeSize _ -> "size of the lattice (LxL)"
            | Seed _ -> "seed of random number generator (default: 1973)"
            | NumberOfStates _ -> "number of states q (default: 2)"
