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

[<Fact>]
let ``get basin`` () =
    Assert.Equal<int * int>([ (0, 0); (0, 1); (1, 0) ], getBasin 1 0 map |> List.sort)

    Assert.Equal<int * int>(
        [ (5, 0)
          (6, 0)
          (7, 0)
          (8, 0)
          (9, 0)
          (6, 1)
          (8, 1)
          (9, 1)
          (9, 2) ]
        |> List.sort,
        getBasin 9 0 map |> List.sort
    )

    Assert.Equal<int * int>(
        [ (2, 1)
          (3, 1)
          (4, 1)
          (1, 2)
          (2, 2)
          (3, 2)
          (4, 2)
          (5, 2)
          (0, 3)
          (1, 3)
          (2, 3)
          (3, 3)
          (4, 3)
          (1, 4) ]
        |> List.sort,
        getBasin 2 2 map |> List.sort
    )

    Assert.Equal<int * int>(
        [ (7, 2)
          (6, 3)
          (7, 3)
          (8, 3)
          (5, 4)
          (6, 4)
          (7, 4)
          (8, 4)
          (9, 4) ]
        |> List.sort,
        getBasin 6 4 map |> List.sort
    )

[<Fact>]
let ``three largest basins`` () =
    Assert.Equal<int>([ 9; 9; 14 ], findThreeLargestBasins map |> List.sort)
