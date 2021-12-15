module Tests

open Day6
open Xunit

[<Fact>]
let ``parse input`` () =
    Assert.Equal<Fish>(parseInput "3,4,3,1,2", [ 0L; 1; 1; 2; 1; 0; 0; 0; 0 ])

[<Fact>]
let ``step`` () =
    Assert.Equal<Fish>(step [ 0; 1; 1; 2; 1; 0; 0; 0; 0 ], [ 1L; 1; 2; 1; 0; 0; 0; 0; 0 ])
    Assert.Equal<Fish>(step [ 1; 1; 2; 1; 0; 0; 0; 0; 0 ], [ 1L; 2; 1; 0; 0; 0; 1; 0; 1 ])
    Assert.Equal<Fish>(step [ 1; 2; 1; 0; 0; 0; 1; 0; 1 ], [ 2L; 1; 0; 0; 0; 1; 1; 1; 1 ])

[<Fact>]
let ``produces the correct result after a number of steps`` () =
    Assert.Equal<Fish>(
        stepTimes 18 [ 0; 1; 1; 2; 1; 0; 0; 0; 0 ]
        |> Seq.sum,
        26
    )

    Assert.Equal<Fish>(
        stepTimes 80 [ 0; 1; 1; 2; 1; 0; 0; 0; 0 ]
        |> Seq.sum,
        5934
    )

    Assert.Equal<Fish>(
        stepTimes 256 [ 0; 1; 1; 2; 1; 0; 0; 0; 0 ]
        |> Seq.sum,
        26984457539L
    )
