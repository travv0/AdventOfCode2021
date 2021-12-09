open System
open System.IO

type Coords = { X: int; Y: int }
type Line = { Start: Coords; End: Coords }

let input = File.ReadAllText("input.txt")

let parseCoords (coords: string) =
    match coords.Split(",") with
    | [| x; y |] -> { X = int x; Y = int y }
    | _ -> failwithf "bad parse for coords `%s`" coords

let parseInput (input: string) =
    let lines =
        input.Split(
            [| "\r\n"; "\n" |],
            StringSplitOptions.RemoveEmptyEntries
            ||| StringSplitOptions.TrimEntries
        )

    [ for line in lines do
          match line.Split(" -> ") with
          | [| lineStart; lineEnd |] ->
              { Start = parseCoords lineStart
                End = parseCoords lineEnd }
          | _ -> failwithf "bad parse for line `%s`" line ]

let lines = parseInput input

let fillLine line =
    if line.Start.X = line.End.X then
        let [ y1; y2 ] =
            [ line.Start.Y; line.End.Y ] |> List.sort

        [ for y in y1 .. y2 -> { X = line.Start.X; Y = y } ]
    elif line.Start.Y = line.End.Y then
        let [ x1; x2 ] =
            [ line.Start.X; line.End.X ] |> List.sort

        [ for x in x1 .. x2 -> { X = x; Y = line.Start.Y } ]
    else
        let xs =
            if line.Start.X < line.End.X then
                [ line.Start.X .. line.End.X ]
            else
                [ line.Start.X .. -1 .. line.End.X ]

        let ys =
            if line.Start.Y < line.End.Y then
                [ line.Start.Y .. line.End.Y ]
            else
                [ line.Start.Y .. -1 .. line.End.Y ]

        [ for x, y in Seq.zip xs ys -> { X = x; Y = y } ]

let isHorizontalOrVertical line =
    line.Start.X = line.End.X
    || line.Start.Y = line.End.Y

let calcOverlaps lines =
    lines
    |> Seq.map fillLine
    |> Seq.concat
    |> Seq.countBy id
    |> Seq.filter (snd >> (<) 1)
    |> Seq.length

lines
|> Seq.filter isHorizontalOrVertical
|> calcOverlaps
|> printfn "Number of points where at least two horizontal or vertical lines overlap: %d"

lines
|> calcOverlaps
|> printfn "Number of points where at least two lines overlap: %d"
