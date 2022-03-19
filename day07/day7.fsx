open System.IO

let parseInput (input: string) =
    input.Trim().Split(',') |> Array.map int

let fileName =
    match fsi.CommandLineArgs |> Array.toList with
    | _ :: fn :: _ -> fn
    | _ -> "input.txt"

let positions = File.ReadAllText(fileName) |> parseInput

let calcCheapestFuelCost fuelUseCalc positions =
    [| Array.min positions .. Array.max positions |]
    |> Array.map (fun i -> positions |> Array.sumBy (fuelUseCalc i))
    |> Array.min

module Part1 =
    let cheapestFuelCost positions =
        calcCheapestFuelCost (fun i pos -> abs (pos - i)) positions

module Part2 =
    let cheapestFuelCost positions =
        calcCheapestFuelCost
            (fun i pos -> Array.init (abs (pos - i)) ((+) 1) |> Array.sum)
            positions

module Tests =
    let run () =
        printfn
            "%A"
            {| Expected = [ 16; 1; 2; 0; 4; 2; 7; 1; 2; 14 ]
               Actual = parseInput "16,1,2,0,4,2,7,1,2,14\n" |}

        printfn
            "%A"
            {| Expected = 37
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
            {| Expected = 168
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
|> printfn
    "The cheapest amount of fuel that can be spent for all crabs to align for part 1 is %d"

Part2.cheapestFuelCost positions
|> printfn
    "The cheapest amount of fuel that can be spent for all crabs to align for part 2 is %d"
