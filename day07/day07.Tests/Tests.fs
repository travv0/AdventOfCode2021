module Tests

open Day7
open Xunit

[<Fact>]
let ``parse input`` () =
    Assert.Equal<int>([ 16; 1; 2; 0; 4; 2; 7; 1; 2; 14 ], parseInput "16,1,2,0,4,2,7,1,2,14\n")

[<Fact>]
let ``cheapest fuel cost part 1`` () =
    Assert.Equal(
        {| Position = 2; Cost = 37 |},
        Part1.cheapestFuelCost [ 16
                                 1
                                 2
                                 0
                                 4
                                 2
                                 7
                                 1
                                 2
                                 14 ]
    )

[<Fact>]
let ``cheapest fuel cost part 2`` () =
    Assert.Equal(
        {| Position = 5; Cost = 168 |},
        Part2.cheapestFuelCost [ 16
                                 1
                                 2
                                 0
                                 4
                                 2
                                 7
                                 1
                                 2
                                 14 ]
    )
