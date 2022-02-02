open System.IO

let fileName =
#if INTERACTIVE
    match fsi.CommandLineArgs |> Array.toList with
    | _ :: fn :: _ -> fn
    | _ ->
#endif
    "input.txt"

let input = File.ReadAllText(fileName)

let inline between min max n = min <= n && n <= max

[<Struct>]
type Cube = { X: int; Y: int; Z: int }

type Cuboid = Set<Cube>

[<Struct>]
type Power =
    | On
    | Off

module Power =
    let parse =
        function
        | "on" -> On
        | "off" -> Off
        | s -> failwithf "bad parse: `%s`" s

[<Struct>]
type RebootStep = RebootStep of Power * Cuboid

type Reactor = Set<Cube>

module Reactor =
    let setCuboid to_ cuboid reactor =
        match to_ with
        | On -> Set.union reactor cuboid
        | Off -> Set.difference reactor cuboid

    let runSteps steps reactor =
        List.fold (fun reactor (RebootStep (to_, cuboid)) -> setCuboid to_ cuboid reactor) steps reactor

[<Struct>]
type BootStage =
    | Initial
    | Reboot

[<Struct>]
type Range = { Start: int; Stop: int }

[<Struct>]
type CuboidRange = { X: Range; Y: Range; Z: Range }

module CuboidRange =
    let parse (s: string) : CuboidRange =
        let parseRange (s: string) =
            match s.Split('=') with
            | [| _; r |] ->
                match r.Split("..") with
                | [| beginning; ending |] ->
                    { Start = int beginning
                      Stop = int ending }
                | _ -> failwithf "bad parse: `%s`" r
            | _ -> failwithf "bad parse: `%s`" s

        match s.Split(',') with
        | [| x; y; z |] ->
            { X = parseRange x
              Y = parseRange y
              Z = parseRange z }
        | _ -> failwithf "bad parse: `%s`" s

    let filter stage range =
        match stage with
        | Initial ->
            if [ range.X; range.Y; range.Z ]
               |> List.forall (fun r ->
                   [ r.Start; r.Stop ]
                   |> List.forall (between (-50) 50)) then
                Some range
            else
                None
        | Reboot -> Some range

    let toCuboid range : Cuboid =
        [ for x in range.X.Start .. range.X.Stop do
              for y in range.Y.Start .. range.Y.Stop do
                  for z in range.Z.Start .. range.Z.Stop do
                      yield { Cube.X = x; Y = y; Z = z } ]
        |> Set.ofList

module RebootStep =
    let parse (stage: BootStage) (s: string) : RebootStep option =
        match s.Split(' ') with
        | [| p; r |] ->
            CuboidRange.parse r
            |> CuboidRange.filter stage
            |> Option.map CuboidRange.toCuboid
            |> Option.map (fun cuboid -> RebootStep(Power.parse p, cuboid))
        | _ -> failwithf "bad parse: `%s`" s

let parseInput (s: string) stage =
    s.Trim().Split("\n")
    |> Seq.choose (RebootStep.parse stage)
    |> Seq.toList

let reactor: Reactor = Set.empty

module Part1 =
    let steps = parseInput input Initial

    Reactor.runSteps reactor steps
    |> Set.count
    |> printfn "After executing the steps in the initialization procedure region, %d cubes are on"

// module Part2 =
//     let steps = parseInput input Reboot
//
//     Reactor.runSteps reactor steps
//     |> Set.count
//     |> printfn "After running the reboot steps, %d cubes are on"
