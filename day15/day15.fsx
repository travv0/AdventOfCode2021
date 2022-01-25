open System
open System.IO
open System.Collections.Generic

let konst a _ = a

let fileName =
    match fsi.CommandLineArgs |> Array.toList with
    | _ :: fn :: _ -> fn
    | _ -> "input.txt"

let input = File.ReadAllText fileName

let getLowestRisk (start: 'a) (goal: 'a) (h: 'a -> int) (neighbors: ('a -> #seq<'a * int>)) =
    let openSet =
        PriorityQueue([ struct (start, h start) ])

    let cameFrom = Dictionary()
    let gScoreMap = Dictionary([ KeyValuePair(start, 0) ])

    let gScore node =
        match gScoreMap.TryGetValue node with
        | (true, v) -> v
        | (false, _) -> Int32.MaxValue

    let mutable result = None

    while Option.isNone result && openSet.Count > 0 do
        let current = openSet.Dequeue()

        if current = goal then
            result <- Some(gScore current)
        else
            for (neighbor, d) in neighbors current do
                let tentativeGScore = gScore current + d

                if tentativeGScore < gScore neighbor then
                    cameFrom.[neighbor] <- current
                    gScoreMap.[neighbor] <- tentativeGScore
                    let fScore = tentativeGScore + h neighbor

                    if not (Seq.exists (fun struct (e, _) -> e = neighbor) openSet.UnorderedItems) then
                        openSet.Enqueue(neighbor, fScore)

    result

type Coords = Coords of int * int

let parseInput (input: string) =
    input.Split(
        '\n',
        StringSplitOptions.RemoveEmptyEntries
        ||| StringSplitOptions.TrimEntries
    )
    |> Array.map (fun line ->
        line
        |> Seq.map (fun c -> sprintf "%c" c |> int)
        |> Seq.toArray)

let cave = parseInput input
let heuristic (Coords (x1, y1)) (Coords (x2, y2)) = abs (x2 - x1) + abs (y2 - y1)
let caveWidth = Array.length cave
let caveHeight = Array.length cave.[0]

let neighbors maxX maxY (Coords (x, y)) : (Coords * int) list =
    [ (-1, 0); (0, -1); (1, 0); (0, 1) ]
    |> List.choose (fun (dx, dy) ->
        let newX = x + dx
        let newY = y + dy

        if newX >= 0
           && newY >= 0
           && newX <= maxX
           && newY <= maxY then
            let tileDistance = (newX / caveWidth) + (newY / caveHeight)

            let shiftedWeight =
                let temp =
                    (cave.[newX % caveWidth].[newY % caveHeight]
                     + tileDistance) % 9

                if temp = 0 then 9 else temp

            Some(Coords(newX, newY), shiftedWeight)
        else
            None)

module Part1 =
    let maxX = caveWidth - 1
    let maxY = caveHeight - 1

    getLowestRisk (Coords(0, 0)) (Coords(maxX, maxY)) (heuristic (Coords(maxX, maxY))) (neighbors maxX maxY)
    |> Option.defaultWith (fun () -> failwith "part 1 failed to find path")
    |> printf
        "The lowest total risk of any path from the top left to the bottom \
        right is %d\n"

module Part2 =
    let maxX = (caveWidth * 5) - 1
    let maxY = (caveHeight * 5) - 1

    getLowestRisk (Coords(0, 0)) (Coords(maxX, maxY)) (heuristic (Coords(maxX, maxY))) (neighbors maxX maxY)
    |> Option.defaultWith (fun () -> failwith "part 2 failed to find path")
    |> printf
        "Using the full map, the lowest total risk of any path from the top \
        left to the bottom right is %d\n"
