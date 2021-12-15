module Tests

open System
open Xunit
open Day3

let input =
    "00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010"
        .Split([| '\n' |])
    |> Array.map (fun s -> s.Trim())

let counts = Part1.counts input

[<Fact>]
let ``gamma rate`` () =
    Assert.Equal(22u, Part1.gammaRate counts)

[<Fact>]
let ``epsilon rate`` () =
    Assert.Equal(9u, Part1.epsilonRate counts)

[<Fact>]
let ``oxygen generator rating`` () =
    Assert.Equal(23u, Part2.oxygenGeneratorRating input)

[<Fact>]
let ``CO2 scrubber rating`` () =
    Assert.Equal(10u, Part2.co2ScrubberRating input)
