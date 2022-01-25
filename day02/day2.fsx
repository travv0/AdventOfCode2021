open System.IO

type Command =
    | Forward
    | Down
    | Up

let parseCommand command =
    match command with
    | "forward" -> Forward
    | "down" -> Down
    | "up" -> Up
    | _ -> failwithf "Invalid command: %s" command

let parseLine (line: string) =
    match line.Split(' ') with
    | [| command; units |] -> Some(string command |> parseCommand, units |> string |> int)
    | _ -> None

let fileName =
    match fsi.CommandLineArgs |> Array.toList with
    | _ :: fn :: _ -> fn
    | _ -> "input.txt"

let commands =
    File.ReadLines(fileName)
    |> Seq.choose parseLine

module Part1 =
    let moveSub (coords: {| Depth: int; Position: int |}) (command, units) =
        match command with
        | Forward ->
            {| coords with
                Position = coords.Position + units |}
        | Down ->
            {| coords with
                Depth = coords.Depth + units |}
        | Up ->
            {| coords with
                Depth = coords.Depth - units |}

    let coords =
        commands
        |> Seq.fold moveSub {| Position = 0; Depth = 0 |}

    printfn "Final horizontal position multiplied by final depth for part 1: %d" (coords.Position * coords.Depth)

module Part2 =
    let moveSub
        (coords: {| Depth: int
                    Position: int
                    Aim: int |})
        (command, units)
        =
        match command with
        | Forward ->
            {| coords with
                Position = coords.Position + units
                Depth = coords.Depth + coords.Aim * units |}
        | Down ->
            {| coords with
                Aim = coords.Aim + units |}
        | Up ->
            {| coords with
                Aim = coords.Aim - units |}

    let coords =
        commands
        |> Seq.fold moveSub {| Position = 0; Depth = 0; Aim = 0 |}

    printfn "Final horizontal position multiplied by final depth for part 2: %d" (coords.Position * coords.Depth)
