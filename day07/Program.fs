module Day7

open System
open System.IO

let parseInput (input: string) =
    input.Trim().Split(',')
    |> Seq.map int
    |> List.ofSeq

let positions =
    File.ReadAllText("input.txt") |> parseInput

let inline cost r = (^T: (member Cost : int) r)

let calcCheapestFuelCost fuelUseCalc positions =
    let mutable cheapestPosition, cheapestCost = -1, Int32.MaxValue

    for i in List.min positions .. List.max positions do
        let cost = positions |> List.sumBy (fuelUseCalc i)

        if cost < cheapestCost then
            cheapestCost <- cost
            cheapestPosition <- i

    {| Position = cheapestPosition
       Cost = cheapestCost |}

module Part1 =
    let cheapestFuelCost positions =
        calcCheapestFuelCost (fun i pos -> abs (pos - i)) positions

    cheapestFuelCost positions
    |> cost
    |> printfn "The cheapest amount of fuel that can be spent for all crabs to align for part 1 is %d"

module Part2 =
    let cheapestFuelCost positions =
        calcCheapestFuelCost (fun i pos -> List.init (abs (pos - i)) ((+) 1) |> List.sum) positions

    cheapestFuelCost positions
    |> cost
    |> printfn "The cheapest amount of fuel that can be spent for all crabs to align for part 2 is %d"
