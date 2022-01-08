open System
open System.IO

let konst a _ = a

let fileName =
    match fsi.CommandLineArgs |> Array.toList with
    | _ :: fn :: _ -> fn
    | _ -> "input.txt"

let input = File.ReadAllText fileName

[<CustomEquality; CustomComparison>]
type Node<'a when 'a :> IComparable and 'a: equality> =
    { Elem: 'a
      F: int }

    override this.Equals(other) =
        match other with
        | :? Node<'a> as n -> this.Elem = n.Elem && this.F = n.F
        | _ -> false

    override this.GetHashCode() =
        this.Elem.GetHashCode() + this.F.GetHashCode()

    interface IComparable with
        member this.CompareTo(other) =
            match other with
            | :? Node<'a> as n -> (this :> IComparable<Node<_>>).CompareTo(n)
            | _ -> -1

    interface IComparable<Node<'a>> with
        member this.CompareTo(other) =
            match this.F.CompareTo(other.F) with
            | 0 -> this.Elem.CompareTo(other.Elem)
            | r -> r

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

let astar (start: 'a) (goal: 'a) (h: 'a -> int) (neighbors: ('a -> 'ns) when 'ns :> seq<'a * int>) =
    let mutable openSet =
        Set.singleton { Elem = start; F = h start }

    let mutable cameFrom = Map.empty
    let mutable gScoreMap = Map.ofList [ (start, 0) ]

    let gScore node =
        Map.tryFind node gScoreMap
        |> Option.defaultValue Int32.MaxValue

    let mutable result = None

    while Option.isNone result && not (Set.isEmpty openSet) do
        let current = Set.minElement openSet

        if current.Elem = goal then
            result <- Some current
        else
            openSet <- Set.remove current openSet

            for (neighbor, d) in neighbors current.Elem do
                let tentativeGScore = gScore current.Elem + d

                if tentativeGScore < gScore neighbor then
                    cameFrom <- Map.change neighbor (konst (Some current)) cameFrom
                    gScoreMap <- Map.change neighbor (konst (Some tentativeGScore)) gScoreMap
                    let fScore = tentativeGScore + h neighbor

                    if not (Set.exists (fun { Elem = e } -> e = neighbor) openSet) then
                        openSet <- Set.add { Elem = neighbor; F = fScore } openSet

    result

let cave = parseInput input
let heuristic (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)
let caveWidth = Array.length cave
let caveHeight = Array.length cave.[0]

let neighbors maxX maxY (x, y) : ((int * int) * int) list =
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

            Some((newX, newY), shiftedWeight)
        else
            None)

module Part1 =
    let maxX = caveWidth - 1
    let maxY = caveHeight - 1

    astar (0, 0) (maxX, maxY) (heuristic (maxX, maxY)) (neighbors maxX maxY)
    |> Option.defaultWith (fun () -> failwith "part 1 failed to find path")
    |> (fun { F = cost } -> cost)
    |> printf
        "The lowest total risk of any path from the top left to the bottom \
        right is %d\n"

module Part2 =
    let maxX = (caveWidth * 5) - 1
    let maxY = (caveHeight * 5) - 1

    astar (0, 0) (maxX, maxY) (heuristic (maxX, maxY)) (neighbors maxX maxY)
    |> Option.defaultWith (fun () -> failwith "part 2 failed to find path")
    |> (fun { F = cost } -> cost)
    |> printf
        "Using the full map, the lowest total risk of any path from the top \
        left to the bottom right is %d\n"