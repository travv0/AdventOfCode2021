open Day3.Part1
open Day3.Part2
open FSharpPlus
open System.IO

[<EntryPoint>]
let main args =
    let fileName =
        args |> tryHead |> Option.defaultValue "input.txt"

    let input = File.ReadLines(fileName)
    let counts = counts input

    printfn "The power consumption of the submarine is %d" (gammaRate counts * epsilonRate counts)

    printfn
        "The life support rating of the submarine is %d"
        (oxygenGeneratorRating input
         * co2ScrubberRating input)

    0
