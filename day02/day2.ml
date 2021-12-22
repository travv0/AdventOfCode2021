open Base
open Stdio
open Printf

type command = Forward | Down | Up

let parse_command command =
  match command with
  | "forward" -> Forward
  | "down" -> Down
  | "up" -> Up
  | _ -> ksprintf failwith "Invalid command: %s" command

let parseLine line =
  match String.split line ~on:' ' with
  | [ command; units ] -> Some (parse_command command, Int.of_string units)
  | _ -> None

let file_name =
  match Sys.get_argv () |> Array.to_list with
  | _ :: fn :: _ -> fn
  | _ -> "input.txt"

let commands = In_channel.read_lines file_name |> List.filter_map ~f:parseLine

module Part1 = struct
  type coords = { depth : int; position : int }

  let moveSub coords (command, units) =
    match command with
    | Forward -> { coords with position = coords.position + units }
    | Down -> { coords with depth = coords.depth + units }
    | Up -> { coords with depth = coords.depth - units }

  let coords =
    commands |> List.fold ~f:moveSub ~init:{ position = 0; depth = 0 }
end

module Part2 = struct
  type coords = { depth : int; position : int; aim : int }

  let moveSub coords (command, units) =
    match command with
    | Forward ->
        { coords with
          position = coords.position + units
        ; depth = coords.depth + (coords.aim * units)
        }
    | Down -> { coords with aim = coords.aim + units }
    | Up -> { coords with aim = coords.aim - units }

  let coords =
    commands |> List.fold ~f:moveSub ~init:{ position = 0; depth = 0; aim = 0 }
end

let () =
  printf "Final horizontal position multiplied by final depth for part 1: %d\n"
    (Part1.coords.position * Part1.coords.depth);

  printf "Final horizontal position multiplied by final depth for part 2: %d\n"
    (Part2.coords.position * Part2.coords.depth)
