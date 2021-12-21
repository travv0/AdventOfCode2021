module Day11

open System
open System.IO

let mutable flashes = 0

let rec incEnergy (octos: int [,]) x y =
    octos.[x, y] <- octos.[x, y] + 1

    if octos.[x, y] = 10 then
        flash octos x y

and flash octos x y =
    flashes <- flashes + 1

    for flashX in max (x - 1) 0 .. min (x + 1) (Array2D.length1 octos - 1) do
        for flashY in max (y - 1) 0 .. min (y + 1) (Array2D.length2 octos - 1) do
            if flashX <> x || flashY <> y then
                incEnergy octos flashX flashY

let step (octos: int [,]) =
    for x in 0 .. Array2D.length1 octos - 1 do
        for y in 0 .. Array2D.length2 octos - 1 do
            if octos.[x, y] < 10 then
                incEnergy octos x y

    for x in 0 .. Array2D.length1 octos - 1 do
        for y in 0 .. Array2D.length2 octos - 1 do
            if octos.[x, y] > 9 then
                octos.[x, y] <- 0

let input = File.ReadAllText("input.txt")

let parse (input: string) : int [,] =
    let lines =
        input.Split(
            '\n',
            StringSplitOptions.RemoveEmptyEntries
            ||| StringSplitOptions.TrimEntries
        )

    array2D [ for line in lines do
                  [ for c in line do
                        sprintf "%c" c |> int ] ]

let runSteps n octos =
    flashes <- 0

    for i in 1 .. n do
        step octos

    flashes

let findSyncronizedFlash octos =
    let mutable stepCount = 0
    let mutable syncronized = false

    while not syncronized do
        step octos
        stepCount <- stepCount + 1
        syncronized <- octos |> Seq.cast |> Seq.forall ((=) 0)

    stepCount

let octos () = parse input

octos ()
|> runSteps 100
|> printfn "After 100 steps, there have been a total of %d flashes"

octos ()
|> findSyncronizedFlash
|> printfn "The first time all octopuses flash simultaneously is step %d"
