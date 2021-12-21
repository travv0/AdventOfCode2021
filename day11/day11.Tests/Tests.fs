module Tests

open Day11
open Xunit

let input =
    "5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526
"

let octos () = parse input

[<Fact>]
let ``1 step`` () = Assert.Equal(0, octos () |> runSteps 1)

[<Fact>]
let ``2 steps`` () =
    Assert.Equal(35, octos () |> runSteps 2)

[<Fact>]
let ``10 steps`` () =
    Assert.Equal(204, octos () |> runSteps 10)

[<Fact>]
let ``100 steps`` () =
    Assert.Equal(1656, octos () |> runSteps 100)

[<Fact>]
let ``find syncronized flash`` () =
    Assert.Equal(195, octos () |> findSyncronizedFlash)
