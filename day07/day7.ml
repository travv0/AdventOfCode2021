open Base
open Stdio

let ( >> ) f g x = g (f x)
let ( << ) f g x = f (g x)

type crab = { position : int; cost : int }

let parse_input (input : string) =
  input |> String.strip |> String.split ~on:',' |> List.map ~f:Int.of_string

let file_name =
  match Sys.get_argv () |> Array.to_list with
  | _ :: fn :: _ -> fn
  | _ -> "input.txt"

let positions = In_channel.read_all file_name |> parse_input
let cost r = r.cost

let calc_cheapest_fuel_cost fuel_use_calc positions =
  let cheapest_position = ref (-1)
  and cheapest_cost = ref Int.max_value
  and start = List.min_elt ~compare:Int.compare positions |> Option.value_exn
  and end_ = List.max_elt ~compare:Int.compare positions |> Option.value_exn in

  for i = start to end_ do
    let cost =
      positions |> List.map ~f:(fuel_use_calc i) |> List.fold ~init:0 ~f:( + )
    in
    if cost < !cheapest_cost then (
      cheapest_cost := cost;
      cheapest_position := i)
  done;

  { cost = !cheapest_cost; position = !cheapest_position }

module Part1 = struct
  let cheapest_fuel_cost positions =
    calc_cheapest_fuel_cost (fun i pos -> abs (pos - i)) positions
end

module Part2 = struct
  let cheapest_fuel_cost positions =
    calc_cheapest_fuel_cost
      (fun i pos ->
        List.init (abs (pos - i)) ~f:(( + ) 1) |> List.fold ~init:0 ~f:( + ))
      positions
end

let () =
  Part1.cheapest_fuel_cost positions
  |> cost
  |> printf
       "The cheapest amount of fuel that can be spent for all crabs to align \
        for part 1 is %d\n";

  Part2.cheapest_fuel_cost positions
  |> cost
  |> printf
       "The cheapest amount of fuel that can be spent for all crabs to align \
        for part 2 is %d\n"
