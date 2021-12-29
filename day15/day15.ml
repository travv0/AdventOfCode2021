open Base
open Stdio
open Printf

let file_name =
  match Sys.get_argv () |> Array.to_list with
  | _ :: fn :: _ -> fn
  | _ -> "input.txt"

let input = In_channel.read_all file_name

module CoordsElem = struct
  module type S = sig
    include Astar.Elem

    val make : int -> int -> t
  end

  let make neighbors =
    (module struct
      module T = struct
        type t = int * int

        let compare (x1, y1) (x2, y2) =
          List.compare Int.compare [ x1; y1 ] [ x2; y2 ]

        let sexp_of_t (x, y) = List.sexp_of_t Int.sexp_of_t [ x; y ]
      end

      include T
      include Comparator.Make (T)

      let make x y : t = (x, y)
      let heuristic (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)
      let ( = ) (x1, y1) (x2, y2) = x1 = x2 && y1 = y2
      let neighbors = neighbors
    end : S)
end

let parse_input input =
  input
  |> String.split_lines
  |> List.map ~f:(fun line ->
         line
         |> String.to_array
         |> Array.map ~f:(fun c -> sprintf "%c" c |> Int.of_string))
  |> List.to_array

let cave = parse_input input
let heuristic (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)
let cave_width = Array.length cave
let cave_height = Array.length cave.(0)

let neighbors max_x max_y (x, y) : ((int * int) * int) list =
  [ (-1, 0); (0, -1); (1, 0); (0, 1) ]
  |> List.filter_map ~f:(fun (dx, dy) ->
         let new_x = x + dx and new_y = y + dy in
         if new_x >= 0 && new_y >= 0 && new_x <= max_x && new_y <= max_y then
           let tile_distance = (new_x / cave_width) + (new_y / cave_height) in
           let shifted_weight =
             let temp =
               (cave.(new_x % cave_width).(new_y % cave_height) + tile_distance)
               % 9
             in
             if temp = 0 then 9 else temp
           in
           Some ((new_x, new_y), shifted_weight)
         else None)

let () =
  let max_x = cave_width - 1 and max_y = cave_height - 1 in
  let module Coords = (val CoordsElem.make (neighbors max_x max_y)) in
  Astar.path (module Coords) (Coords.make 0 0) (Coords.make max_x max_y)
  |> Option.value_exn
  |> snd
  |> printf
       "The lowest total risk of any path from the top left to the bottom \
        right is %d\n"

let () =
  let max_x = (cave_width * 5) - 1 and max_y = (cave_height * 5) - 1 in
  let module Coords = (val CoordsElem.make (neighbors max_x max_y)) in
  Astar.path (module Coords) (Coords.make 0 0) (Coords.make max_x max_y)
  |> Option.value_exn
  |> snd
  |> printf
       "Using the full map, the lowest total risk of any path from the top \
        left to the bottom right is %d\n"
