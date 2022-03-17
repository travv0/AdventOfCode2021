open System
open System.IO

let fileName =
    match fsi.CommandLineArgs |> Array.toList with
    | _ :: fn :: _ -> fn
    | _ -> "input.txt"

let input = File.ReadAllText(fileName)

module Octos =
    type private Octos = int [,]

    let parse (input: string) : Octos =
        let lines =
            input.Split(
                '\n',
                StringSplitOptions.RemoveEmptyEntries
                ||| StringSplitOptions.TrimEntries
            )

        array2D [ for line in lines do
                      [ for c in line do
                            sprintf "%c" c |> int ] ]

    let mutable private flashes = 0

    let rec private incEnergy (octos: Octos) x y =
        octos.[x, y] <- octos.[x, y] + 1

        if octos.[x, y] = 10 then
            flash octos x y

    and private flash octos x y =
        flashes <- flashes + 1

        for flashX in max (x - 1) 0 .. min (x + 1) (Array2D.length1 octos - 1) do
            for flashY in
                max (y - 1) 0 .. min (y + 1) (Array2D.length2 octos - 1) do
                if flashX <> x || flashY <> y then
                    incEnergy octos flashX flashY

    let private step (octos: Octos) =
        for x in 0 .. Array2D.length1 octos - 1 do
            for y in 0 .. Array2D.length2 octos - 1 do
                if octos.[x, y] < 10 then
                    incEnergy octos x y

        for x in 0 .. Array2D.length1 octos - 1 do
            for y in 0 .. Array2D.length2 octos - 1 do
                if octos.[x, y] > 9 then
                    octos.[x, y] <- 0

    let runSteps n input =
        let octos = parse input
        flashes <- 0

        for _ in 1 .. n do
            step octos

        flashes

    let findSynchronizedFlash input =
        let octos = parse input
        let mutable stepCount = 0
        let mutable synchronized = false

        while not synchronized do
            step octos
            stepCount <- stepCount + 1
            synchronized <- octos |> Seq.cast |> Seq.forall ((=) 0)

        stepCount

Octos.runSteps 100 input
|> printfn "After 100 steps, there have been a total of %d flashes"

Octos.findSynchronizedFlash input
|> printfn "The first time all octopuses flash simultaneously is step %d"

module Tests =
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

    let octos () = Octos.parse input

    let run () =
        printfn
            "%A"
            {| Expected = 0
               Actual = Octos.runSteps 1 |}

        printfn
            "%A"
            {| Expected = 35
               Actual = Octos.runSteps 2 |}

        printfn
            "%A"
            {| Expected = 204
               Actual = Octos.runSteps 10 |}

        printfn
            "%A"
            {| Expected = 1656
               Actual = Octos.runSteps 100 |}

        printfn
            "%A"
            {| Expected = 195
               Actual = Octos.findSynchronizedFlash |}
