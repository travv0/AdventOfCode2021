module Tests

open Day6
open Xunit

[<Fact>]
let ``parse input`` () =
    Assert.Equal<Fish>([ 0L; 1; 1; 2; 1; 0; 0; 0; 0 ], parseInput "3,4,3,1,2")

[<Fact>]
let ``step`` () =
    Assert.Equal<Fish>([ 1L; 1; 2; 1; 0; 0; 0; 0; 0 ], step [ 0; 1; 1; 2; 1; 0; 0; 0; 0 ])
    Assert.Equal<Fish>([ 1L; 2; 1; 0; 0; 0; 1; 0; 1 ], step [ 1; 1; 2; 1; 0; 0; 0; 0; 0 ])
    Assert.Equal<Fish>([ 2L; 1; 0; 0; 0; 1; 1; 1; 1 ], step [ 1; 2; 1; 0; 0; 0; 1; 0; 1 ])

[<Fact>]
let ``produces the correct result after a number of steps`` () =
    Assert.Equal<Fish>(
        26,
        stepTimes 18 [ 0; 1; 1; 2; 1; 0; 0; 0; 0 ]
        |> Seq.sum
    )

    Assert.Equal<Fish>(
        5934,
        stepTimes 80 [ 0; 1; 1; 2; 1; 0; 0; 0; 0 ]
        |> Seq.sum
    )

    Assert.Equal<Fish>(
        26984457539L,
        stepTimes 256 [ 0; 1; 1; 2; 1; 0; 0; 0; 0 ]
        |> Seq.sum
    )
