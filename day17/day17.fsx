open System.IO
open System.Text.RegularExpressions

let fileName =
    match fsi.CommandLineArgs |> Array.toList with
    | _ :: fn :: _ -> fn
    | _ -> "input.txt"

let input = File.ReadAllText(fileName)

type Probe =
    { X: int
      Y: int
      XVelocity: int
      YVelocity: int }

module Probe =
    let make xVelocity yVelocity =
        { X = 0
          Y = 0
          XVelocity = xVelocity
          YVelocity = yVelocity }

    let step probe =
        { X = probe.X + probe.XVelocity
          Y = probe.Y + probe.YVelocity
          XVelocity =
            if probe.XVelocity > 0 then
                probe.XVelocity - 1
            else if probe.XVelocity < 0 then
                probe.XVelocity + 1
            else
                0
          YVelocity = probe.YVelocity - 1 }

    let hitsTarget minX maxX minY maxY probe : option<int> =
        let rec hitsTarget' probe peakY =
            if probe.Y < minY && probe.YVelocity < 0 then
                None
            else if minX <= probe.X
                    && probe.X <= maxX
                    && minY <= probe.Y
                    && probe.Y <= maxY then
                Some peakY
            else
                let nextProbe = step probe
                hitsTarget' nextProbe (max peakY nextProbe.Y)

        hitsTarget' probe probe.Y

type ParseResult =
    { MinX: int
      MaxX: int
      MinY: int
      MaxY: int }

let parseInput input =
    match Regex.Matches(input, "(-?[0-9]+)") |> Seq.toList with
    | [ minX; maxX; minY; maxY ] ->
        { MinX = int minX.Value
          MaxX = int maxX.Value
          MinY = int minY.Value
          MaxY = int maxY.Value }
    | _ -> failwithf "bad parse: %s" input

let { MinX = minX
      MaxX = maxX
      MinY = minY
      MaxY = maxY } =
    parseInput input

let yRange = max (abs minY) (abs maxY)

let hits =
    [ for x in 0 .. maxX do
          yield!
              [ for y in (-yRange) .. yRange do
                    match Probe.make x y
                          |> Probe.hitsTarget minX maxX minY maxY
                        with
                    | Some peakY -> yield peakY
                    | None -> () ] ]

hits
|> List.max
|> printfn "The highest y position the probe can reach is %d"

hits
|> List.length
|> printfn "There are %d distinct initial velocity values that cause the probe to fall within the target area"
