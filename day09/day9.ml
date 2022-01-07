open Base
open Printf
open Stdio

let failwithf f = ksprintf failwith f
let ( >> ) f g x = g (f x)
let ( << ) f g x = f (g x)
let ( ||> ) (a, b) f = f a b

module Int_pair = struct
  module T = struct
    type t = int * int [@@deriving compare, sexp_of]
  end

  include T
  include Comparator.Make (T)
end

let file_name =
  match Sys.get_argv () |> Array.to_list with
  | _ :: fn :: _ -> fn
  | _ -> "input.txt"

let get_adjacent_locations (x : int) (y : int) (map : int array array) =
  [ (-1, 0); (0, -1); (1, 0); (0, 1) ]
  |> List.map ~f:(fun (dx, dy) -> (x + dx, y + dy))
  |> List.filter ~f:(fun (adj_x, adj_y) ->
         adj_x >= 0
         && adj_x < Array.length map
         && adj_y >= 0
         && adj_y < Array.length map.(x))
  |> Set.of_list (module Int_pair)

let parse_input (input : string) =
  input
  |> String.split_lines
  |> List.map
       ~f:(String.to_array >> Array.map ~f:(sprintf "%c" >> Int.of_string))
  |> Array.of_list
  |> Array.transpose_exn

let find_low_points map =
  let mark_low_point map x y _ =
    let adjacent_locations =
      get_adjacent_locations x y map
      |> Set.map (module Int) ~f:(fun (adj_x, adj_y) -> map.(adj_x).(adj_y))
    in
    if Set.for_all ~f:(( < ) map.(x).(y)) adjacent_locations then
      Some map.(x).(y)
    else None
  in

  let low_points =
    Array.mapi map ~f:(fun x col -> Array.mapi col ~f:(mark_low_point map x))
  in

  List.range 0 (Array.length map)
  |> List.map ~f:(fun x ->
         List.range 0 (Array.length map.(x))
         |> List.filter_map ~f:(fun y ->
                match low_points.(x).(y) with
                | Some _ -> Some (x, y)
                | None -> None))
  |> List.concat

let find_risk_levels map =
  map |> find_low_points |> List.map ~f:(fun (x, y) -> map.(x).(y) + 1)

let map = In_channel.read_all file_name |> parse_input

let get_basin x y map =
  let seen = ref @@ Set.of_list (module Int_pair) [ (x, y) ] in

  let get_basin_adjacents x y =
    get_adjacent_locations x y map
    |> Set.filter ~f:(fun (adj_x, adj_y) ->
           map.(adj_x).(adj_y) <> 9
           && not (!seen |> Set.exists ~f:(fun (x, y) -> x = adj_x && y = adj_y)))
  in

  let locations_to_check = ref @@ get_basin_adjacents x y in

  while Set.length !locations_to_check > 0 do
    let locs = !locations_to_check |> Set.to_list in
    let location = List.hd_exn locs in
    locations_to_check :=
      Set.union
        (location ||> get_basin_adjacents)
        (List.tl_exn locs |> Set.of_list (module Int_pair));
    seen := Set.add !seen location
  done;

  Set.to_list !seen

let find_three_largest_basins map =
  let sorted_basins =
    find_low_points map
    |> List.map ~f:(fun (x, y) -> get_basin x y map |> List.length)
    |> List.sort ~compare:(fun a b -> neg @@ Int.compare a b)
  in
  List.take sorted_basins 3

let () =
  find_risk_levels map
  |> List.fold ~init:0 ~f:( + )
  |> printf
       "The sum of the risk levels of all low points in the heightmap is %d\n";

  find_three_largest_basins map
  |> List.fold ~init:1 ~f:( * )
  |> printf "The product of the sizes of the three largest basins is %d\n"
