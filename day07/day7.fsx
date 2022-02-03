open System
open System.IO

let parseInput (input: string) =
    input.Trim().Split(',') |> Array.map int

let fileName =
    match fsi.CommandLineArgs |> Array.toList with
    | _ :: fn :: _ -> fn
    | _ -> "input.txt"

let positions = File.ReadAllText(fileName) |> parseInput

let inline cost r = (^T: (member Cost : int) r)

let calcCheapestFuelCost fuelUseCalc positions =
    let mutable cheapestPosition, cheapestCost = -1, Int32.MaxValue

    for i in Array.min positions .. Array.max positions do
        let cost = positions |> Array.sumBy (fuelUseCalc i)

        if cost < cheapestCost then
            cheapestCost <- cost
            cheapestPosition <- i

    {| Position = cheapestPosition
       Cost = cheapestCost |}

module Part1 =
    let cheapestFuelCost positions =
        calcCheapestFuelCost (fun i pos -> abs (pos - i)) positions

module Part2 =
    let cheapestFuelCost positions =
        calcCheapestFuelCost (fun i pos -> Array.init (abs (pos - i)) ((+) 1) |> Array.sum) positions

module Tests =
    let run () =
        printfn
            "%A"
            {| Expected = [ 16; 1; 2; 0; 4; 2; 7; 1; 2; 14 ]
               Actual = parseInput "16,1,2,0,4,2,7,1,2,14\n" |}

        printfn
            "%A"
            {| Expected = {| Position = 2; Cost = 37 |}
               Actual =
                Part1.cheapestFuelCost [| 16
                                          1
                                          2
                                          0
                                          4
                                          2
                                          7
                                          1
                                          2
                                          14 |] |}

        printfn
            "%A"
            {| Expected = {| Position = 5; Cost = 168 |}
               Actual =
                Part2.cheapestFuelCost [| 16
                                          1
                                          2
                                          0
                                          4
                                          2
                                          7
                                          1
                                          2
                                          14 |] |}

Part1.cheapestFuelCost positions
|> cost
|> printfn "The cheapest amount of fuel that can be spent for all crabs to align for part 1 is %d"

Part2.cheapestFuelCost positions
|> cost
|> printfn "The cheapest amount of fuel that can be spent for all crabs to align for part 2 is %d"
