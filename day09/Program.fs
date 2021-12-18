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
    |> Seq.map (fun (adjX, adjY) -> map.[adjX, adjY])
    |> Seq.toList

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

let markLowPoint map x y _ =
    let adjacentLocations = getAdjacentLocations x y map

    if Seq.forall ((<) map.[x, y]) adjacentLocations then
        Some(map.[x, y])
    else
        None

let findRiskLevels map =
    map
    |> Array2D.mapi (markLowPoint map)
    |> Seq.cast
    |> Seq.choose (Option.map ((+) 1))
    |> Seq.toList

let map =
    File.ReadAllText("input.txt") |> parseInput

findRiskLevels map
|> List.sum
|> printfn "The sum of the risk levels of all low points in the heightmap is %d"
