open System
open System.IO
open System.Collections.Generic

let konst a _ = a
let flip f x y = f y x

let fileName =
    match fsi.CommandLineArgs |> Array.toList with
    | _ :: fn :: _ -> fn
    | _ -> "input.txt"

let input = File.ReadAllText fileName

type Neighbor<'a> = { Node: 'a; Weight: int }

let astar
    (start: 'a)
    (goal: 'a)
    (h: 'a -> int)
    (neighbors: 'a -> #seq<Neighbor<'a>>)
    =
    let openSet =
        PriorityQueue([ struct (start, h start) ])

    let cameFrom = Dictionary()
    let gScoreMap = Dictionary([ KeyValuePair(start, 0) ])

    let gScore node =
        match gScoreMap.TryGetValue node with
        | true, v -> v
        | false, _ -> Int32.MaxValue

    let mutable result = None

    while Option.isNone result && openSet.Count > 0 do
        let current = openSet.Dequeue()

        if current = goal then
            result <- Some(current, gScore current)
        else
            for { Node = neighbor; Weight = d } in neighbors current do
                let tentativeGScore = gScore current + d

                if tentativeGScore < gScore neighbor then
                    cameFrom.[neighbor] <- current
                    gScoreMap.[neighbor] <- tentativeGScore
                    let fScore = tentativeGScore + h neighbor

                    if
                        not
                            (
                                Seq.exists
                                    (fun struct (e, _) -> e = neighbor)
                                    openSet.UnorderedItems
                            )
                    then
                        openSet.Enqueue(neighbor, fScore)

    result

type Coords = { X: int; Y: int }
let (|Coords|) (coords: Coords) = (coords.X, coords.Y)

module Coords =
    let make x y = { X = x; Y = y }
    let zero = make 0 0

let parseInput (input: string) =
    input.Split(
        '\n',
        StringSplitOptions.RemoveEmptyEntries
        ||| StringSplitOptions.TrimEntries
    )
    |> Array.map
        (fun line ->
            line
            |> Seq.map (fun c -> sprintf "%c" c |> int)
            |> Seq.toArray)
    |> array2D

let cave = parseInput input

let heuristic (Coords (x1, y1)) (Coords (x2, y2)) =
    abs (x2 - x1) + abs (y2 - y1)

let caveWidth = Array2D.length2 cave
let caveHeight = Array2D.length1 cave

let neighbors maxX maxY (Coords (x, y)) =
    [ (-1, 0); (0, -1); (1, 0); (0, 1) ]
    |> List.choose
        (fun (dx, dy) ->
            let newX = x + dx
            let newY = y + dy

            if newX >= 0
               && newY >= 0
               && newX <= maxX
               && newY <= maxY then
                let tileDistance = (newX / caveWidth) + (newY / caveHeight)

                let shiftedWeight =
                    let temp =
                        (cave.[newY % caveHeight, newX % caveWidth]
                         + tileDistance) % 9

                    if temp = 0 then 9 else temp

                Some
                    { Node = Coords.make newX newY
                      Weight = shiftedWeight }
            else
                None)

let getLowestRisk goal =
    astar Coords.zero goal (flip heuristic goal) (neighbors goal.X goal.Y)
    |> Option.map snd

let makeGoal mapWidth = Coords.make mapWidth mapWidth

module Part1 =
    makeGoal (caveWidth - 1)
    |> getLowestRisk
    |> Option.defaultWith (fun () -> failwith "part 1 failed to find path")
    |> printfn
        "The lowest total risk of any path from the top left to the bottom right is %d"

module Part2 =
    makeGoal ((caveWidth * 5) - 1)
    |> getLowestRisk
    |> Option.defaultWith (fun () -> failwith "part 2 failed to find path")
    |> printfn
        "Using the full map, the lowest total risk of any path from the top left to the bottom right is %d"
