open System.IO

let processLine (line: string) =
    match line.Trim().Split([| ' ' |]) with
    | [| command; units |] -> Some(string command, units |> string |> int)
    | _ -> None

let commands =
    File.ReadLines("input.txt")
    |> Seq.choose processLine

module Part1 =
    let moveSub (coords: {| depth: int; position: int |}) (command, units) =
        match command with
        | "forward" ->
            {| coords with
                position = coords.position + units |}
        | "down" ->
            {| coords with
                depth = coords.depth + units |}
        | "up" ->
            {| coords with
                depth = coords.depth - units |}
        | _ -> failwithf "Invalid command: %s" command

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
        | "forward" ->
            {| coords with
                position = coords.position + units
                depth = coords.depth + coords.aim * units |}
        | "down" ->
            {| coords with
                aim = coords.aim + units |}
        | "up" ->
            {| coords with
                aim = coords.aim - units |}
        | _ -> failwithf "Invalid command: %s" command

    let coords =
        commands
        |> Seq.fold moveSub {| position = 0; depth = 0; aim = 0 |}

    printfn "Final horizontal position multiplied by final depth for part 2: %d" (coords.position * coords.depth)
