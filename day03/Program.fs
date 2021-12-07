open System.IO
open Day3

[<EntryPoint>]
let main args =
    let fileName =
        args
        |> Array.tryHead
        |> Option.defaultValue "input.txt"

    let input = File.ReadLines(fileName)
    let counts = Part1.counts input

    printfn "The power consumption of the submarine is %d" (Part1.gammaRate counts * Part1.epsilonRate counts)

    printfn
        "The life support rating of the submarine is %d"
        (Part2.oxygenGeneratorRating input
         * Part2.co2ScrubberRating input)

    0
