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
    match line.Trim().Split([| ' ' |]) with
    | [| command; units |] -> Some(string command |> parseCommand, units |> string |> int)
    | _ -> None

let commands =
    File.ReadLines("input.txt")
    |> Seq.choose parseLine

module Part1 =
    let moveSub (coords: {| depth: int; position: int |}) (command, units) =
        match command with
        | Forward ->
            {| coords with
                position = coords.position + units |}
        | Down ->
            {| coords with
                depth = coords.depth + units |}
        | Up ->
            {| coords with
                depth = coords.depth - units |}

    let coords =
        commands
        |> Seq.fold moveSub {| position = 0; depth = 0 |}

    printfn "Final horizontal position multiplied by final depth for part 1: %d" (coords.position * coords.depth)

module Part2 =
    let moveSub
        (coords: {| depth: int
                    position: int
                    aim: int |})
        (command, units)
        =
        match command with
        | Forward ->
            {| coords with
                position = coords.position + units
                depth = coords.depth + coords.aim * units |}
        | Down ->
            {| coords with
                aim = coords.aim + units |}
        | Up ->
            {| coords with
                aim = coords.aim - units |}

    let coords =
        commands
        |> Seq.fold moveSub {| position = 0; depth = 0; aim = 0 |}

    printfn "Final horizontal position multiplied by final depth for part 2: %d" (coords.position * coords.depth)
