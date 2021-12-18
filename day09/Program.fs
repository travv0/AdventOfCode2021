module Day9

open System
open System.IO

let getAdjacentLocations (x: int) (y: int) (map: int [,]) =
    seq {
        -1, 0
        0, -1
        1, 0
        0, 1
    }
    |> Seq.map (fun (dx, dy) -> x + dx, y + dy)
    |> Seq.filter (fun (adjX, adjY) ->
        adjX >= 0
        && adjX < Array2D.length1 map
        && adjY >= 0
        && adjY < Array2D.length2 map)

module Array2D =
    let transpose array =
        Array2D.init (Array2D.length2 array) (Array2D.length1 array) (fun r c -> array.[c, r])

let parseInput (input: string) =
    input.Split(
        '\n',
        StringSplitOptions.RemoveEmptyEntries
        ||| StringSplitOptions.TrimEntries
    )
    |> Seq.ofArray
    |> Seq.map (Array.ofSeq >> Seq.map (sprintf "%c" >> int))
    |> array2D
    |> Array2D.transpose

let findLowPoints map =
    let markLowPoint map x y _ =
        let adjacentLocations =
            getAdjacentLocations x y map
            |> Seq.map (fun (adjX, adjY) -> map.[adjX, adjY])

        if Seq.forall ((<) map.[x, y]) adjacentLocations then
            Some(map.[x, y])
        else
            None

    let lowPoints = map |> Array2D.mapi (markLowPoint map)

    seq {
        for x in 0 .. Array2D.length1 map - 1 do
            for y in 0 .. Array2D.length2 map - 1 do
                if Option.isSome lowPoints.[x, y] then
                    yield x, y
    }

let findRiskLevels map =
    map
    |> findLowPoints
    |> Seq.map (fun (x, y) -> map.[x, y] + 1)
    |> Seq.toList

let map =
    File.ReadAllText("input.txt") |> parseInput

findRiskLevels map
|> List.sum
|> printfn "The sum of the risk levels of all low points in the heightmap is %d"

let getBasin x y map =
    let mutable seen = Set.ofList [ x, y ]

    let getBasinAdjacents x y =
        getAdjacentLocations x y map
        |> Seq.filter (fun (adjX, adjY) ->
            map.[adjX, adjY] <> 9
            && not (seen |> Set.contains (adjX, adjY)))
        |> Set.ofSeq

    let mutable locationsToCheck = getBasinAdjacents x y

    while Set.count locationsToCheck > 0 do
        let locSeq = locationsToCheck |> Set.toSeq
        let location = Seq.head locSeq
        locationsToCheck <- Set.union (location ||> getBasinAdjacents) (Seq.tail locSeq |> Set.ofSeq)
        seen <- Set.add location seen

    List.ofSeq seen

let findThreeLargestBasins map =
    findLowPoints map
    |> Seq.map (fun (x, y) -> getBasin x y map |> Seq.length)
    |> Seq.sortDescending
    |> Seq.take 3
    |> Seq.toList

findThreeLargestBasins map
|> List.reduce (*)
|> printfn "The product of the sizes of the three largest basins is %d"
