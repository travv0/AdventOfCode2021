open System.IO

type Fish = int64
type Fishes = list<Fish>

module Seq =
    module Assoc =
        let lookup elem source =
            source
            |> Seq.tryFind (fst >> (=) elem)
            |> Option.map snd

let parseInput (input: string) : Fishes =
    let fishNums = input.Trim().Split(',')
    let counts = Seq.countBy int fishNums

    [ for i in 0 .. 8 do
          counts
          |> Seq.Assoc.lookup i
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

let fileName =
    match fsi.CommandLineArgs |> Array.toList with
    | _ :: fn :: _ -> fn
    | _ -> "input.txt"

let fishes = File.ReadAllText(fileName) |> parseInput

stepTimes 80 fishes
|> Seq.sum
|> printfn "Number of lanternfish after 80 days: %d"

stepTimes 256 fishes
|> Seq.sum
|> printfn "Number of lanternfish after 256 days: %d"

module Tests =
    let printResult expected actual =
        printfn
            "%A"
            {| Expected = expected
               Actual = actual
               Equal = expected = actual |}

    let run () =
        printResult (parseInput "3,4,3,1,2") [ 0; 1; 1; 2; 1; 0; 0; 0; 0 ]

        printResult
            (step [ 0; 1; 1; 2; 1; 0; 0; 0; 0 ])
            [ 1; 1; 2; 1; 0; 0; 0; 0; 0 ]

        printResult
            (step [ 1; 1; 2; 1; 0; 0; 0; 0; 0 ])
            [ 1; 2; 1; 0; 0; 0; 1; 0; 1 ]

        printResult
            (step [ 1; 2; 1; 0; 0; 0; 1; 0; 1 ])
            [ 2; 1; 0; 0; 0; 1; 1; 1; 1 ]

        printResult
            (stepTimes 18 [ 0; 1; 1; 2; 1; 0; 0; 0; 0 ]
             |> Seq.sum)
            26

        printResult
            (stepTimes 80 [ 0; 1; 1; 2; 1; 0; 0; 0; 0 ]
             |> Seq.sum)
            5934

        printResult
            (stepTimes 256 [ 0; 1; 1; 2; 1; 0; 0; 0; 0 ]
             |> Seq.sum)
            26984457539L
