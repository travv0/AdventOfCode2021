module Tests

open Day9
open Xunit

let map =
    parseInput
        "2199943210
3987894921
9856789892
8767896789
9899965678"

[<Fact>]
let ``low points`` () =
    Assert.Equal<int>([ 1; 2; 6; 6 ], findRiskLevels map |> List.sort)
