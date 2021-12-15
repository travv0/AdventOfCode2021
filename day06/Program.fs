module Day6

open System.IO

type Fish = int64
type Fishes = list<Fish>

let parseInput (input: string) : Fishes =
    let fishNums = input.Trim().Split(',')
    let counts = Seq.countBy int fishNums

    [ for i in 0 .. 8 do
          counts
          |> Seq.tryFind ((=) i << fst)
          |> Option.map snd
          |> Option.defaultValue 0
          |> int64 ]

module List =
    let adjustAt index f source =
        match List.splitAt index source with
        | head, elem :: tail -> head @ f elem :: tail
        | _ -> source

let step (fishes: Fishes) : Fishes =
    match fishes with
    | zeros :: fishes -> List.adjustAt 6 ((+) zeros) fishes @ [ zeros ]
    | _ -> failwith "where your fishes at?"

let rec stepTimes (n: int) (fishes: Fishes) : Fishes =
    match n with
    | 0 -> fishes
    | n -> stepTimes (n - 1) (step fishes)

let fishes =
    File.ReadAllText("input.txt") |> parseInput

stepTimes 80 fishes
|> Seq.sum
|> printfn "Number of lanternfish after 80 days: %d"

stepTimes 256 fishes
|> Seq.sum
|> printfn "Number of lanternfish after 256 days: %d"
