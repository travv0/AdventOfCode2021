open System
open System.IO

type Coords = { X: int; Y: int }

type Axis =
    | X
    | Y
    static member Parse =
        function
        | "X"
        | "x" -> X
        | "Y"
        | "y" -> Y
        | a -> failwithf "bad parse: %s" a

type Paper = Set<Coords>

type Fold = { Axis: Axis; Coord: int }
type ParseResult = { Paper: Paper; FoldQueue: list<Fold> }

let input = File.ReadAllText("input.txt")

let parseCoords (s: string) =
    match s.Split(',') with
    | [| x; y |] -> { X = int x; Y = int y }
    | _ -> failwithf "bad parse: %s" s

let parseFold (s: string) =
    match s.Split(' ') |> Array.map (fun s -> s.Split('=')) with
    | [| _; _; [| axis; coord |] |] ->
        { Axis = Axis.Parse(axis)
          Coord = int coord }
    | _ -> failwithf "bad parse: %s" s

let parseInput (input: string) : ParseResult =
    match input.Trim().Split("\n\n") with
    | [| coords; folds |] ->
        let paper =
            coords.Split('\n')
            |> Array.map parseCoords
            |> Set.ofArray

        let foldQueue =
            folds.Split('\n')
            |> Array.map parseFold
            |> Array.toList

        { Paper = paper; FoldQueue = foldQueue }
    | _ -> failwithf "bad parse: %s" input

let foldHorizontal foldY (paper: Paper) : Paper =
    let wereDots, areDots =
        Set.partition (fun c -> c.Y > foldY) paper

    wereDots
    |> Set.map (fun ({ Y = y } as coords) -> { coords with Y = foldY - (y - foldY) })
    |> Set.union areDots


let foldVertical foldX (paper: Paper) : Paper =
    let wereDots, areDots =
        Set.partition (fun c -> c.X > foldX) paper

    wereDots
    |> Set.map (fun ({ X = x } as coords) -> { coords with X = foldX - (x - foldX) })
    |> Set.union areDots

let printPaper paper =
    let xs, ys =
        paper
        |> Set.toList
        |> List.map (fun { X = x; Y = y } -> (x, y))
        |> List.unzip

    let maxX = List.max xs
    let maxY = List.max ys

    for y = 0 to maxY do
        for x = 0 to maxX do
            if Set.exists (fun { X = cx; Y = cy } -> cx = x && cy = y) paper then
                printf "#"
            else
                printf "."

        printf "\n"

let { Paper = paper; FoldQueue = foldQueue } = parseInput input

let rec foldPaper foldQueue paper =
    match foldQueue with
    | { Axis = axis; Coord = coord } :: foldQueue ->
        let foldFn =
            match axis with
            | X -> foldVertical
            | Y -> foldHorizontal

        foldPaper foldQueue (foldFn coord paper)
    | _ -> paper

paper
|> foldPaper [ List.head foldQueue ]
|> Set.count
|> printf "After the first fold, only %d dots are visible\n"

printf "The code to activate the infrared thermal imaging camera system is\n"
paper |> foldPaper foldQueue |> printPaper
