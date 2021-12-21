open Base
open Printf
open Stdio

let flashes = ref 0
let x_length (octos : 'a array array) = Array.length octos
let y_length (octos : 'a array array) = Array.length octos.(0)

let rec inc_energy octos x y =
  octos.(x).(y) <- octos.(x).(y) + 1;
  if octos.(x).(y) = 10 then flash octos x y

and flash octos x y =
  flashes := !flashes + 1;
  for flash_x = max (x - 1) 0 to min (x + 1) (x_length octos - 1) do
    for flash_y = max (y - 1) 0 to min (y + 1) (y_length octos - 1) do
      if flash_x <> x || flash_y <> y then inc_energy octos flash_x flash_y
    done
  done

let step octos =
  for x = 0 to x_length octos - 1 do
    for y = 0 to y_length octos - 1 do
      if octos.(x).(y) < 10 then inc_energy octos x y
    done
  done;

  for x = 0 to x_length octos - 1 do
    for y = 0 to y_length octos - 1 do
      if octos.(x).(y) > 9 then octos.(x).(y) <- 0
    done
  done

let file_name =
  match Sys.get_argv () |> Array.to_list with
  | _ :: fn :: _ -> fn
  | _ -> "input.txt"

let input = In_channel.read_all file_name

let parse input =
  let lines = String.split_lines input in
  let octos =
    Array.make_matrix ~dimx:(List.length lines)
      ~dimy:(String.length (List.hd_exn lines))
      0
  in
  List.iteri lines ~f:(fun x line ->
      line
      |> String.to_array
      |> Array.iteri ~f:(fun y c ->
             octos.(x).(y) <- sprintf "%c" c |> Int.of_string));
  octos

let run_steps n octos =
  flashes := 0;
  for _ = 1 to n do
    step octos
  done;
  !flashes

let find_syncronized_flash octos =
  let step_count = ref 0 and syncronized = ref false in
  while not !syncronized do
    step octos;
    step_count := !step_count + 1;
    syncronized :=
      octos |> Array.for_all ~f:(fun col -> col |> Array.for_all ~f:(( = ) 0))
  done;
  !step_count

let octos () = parse input

let () =
  octos ()
  |> run_steps 100
  |> printf "After 100 steps, there have been a total of %d flashes\n";

  octos ()
  |> find_syncronized_flash
  |> printf "The first time all octopuses flash simultaneously is step %d\n"
