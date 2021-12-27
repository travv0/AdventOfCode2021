open Base
open Stdio
open Printf

let file_name =
  match Sys.get_argv () |> Array.to_list with
  | _ :: fn :: _ -> fn
  | _ -> "input.txt"

let input = In_channel.read_all file_name

type node = (int * int) * int

module Node = struct
  module T = struct
    type t = node

    let compare ((x1, y1), f1) ((x2, y2), f2) =
      List.compare Int.compare [ f1; x1; y1 ] [ f2; x2; y2 ]

    let sexp_of_t ((x, y), f) = List.sexp_of_t Int.sexp_of_t [ x; y; f ]
  end

  include T
  include Comparator.Make (T)
end

module Coords = struct
  module T = struct
    type t = int * int

    let compare (x1, y1) (x2, y2) =
      List.compare Int.compare [ x1; y1 ] [ x2; y2 ]

    let sexp_of_t (x, y) = List.sexp_of_t Int.sexp_of_t [ x; y ]
  end

  include T
  include Comparator.Make (T)
end

let parse_input input =
  input |> String.split_lines
  |> List.map ~f:(fun line ->
         line |> String.to_array
         |> Array.map ~f:(fun c -> sprintf "%c" c |> Int.of_string))
  |> List.to_array

let cave = parse_input input
let heuristic (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

let astar node_comp elem_comp ~(start : 'a) ~(goal : 'a) ~(h : 'a -> int)
    ~(eq : 'a -> 'a -> bool) ~(neighbors : 'a -> ('a * int) list) =
  let open_set = ref @@ Set.singleton node_comp (start, h start) in
  let came_from = ref @@ Map.empty elem_comp in

  let g_score_map = ref @@ Map.singleton elem_comp start 0 in
  let g_score node =
    match Map.find !g_score_map node with Some g -> g | None -> Int.max_value
  in

  let result = ref None in

  while Option.is_none !result && (not @@ Set.is_empty !open_set) do
    let current = Set.min_elt_exn !open_set in
    if eq (fst current) goal then result := Some current
    else (
      open_set := Set.remove_index !open_set 0;
      current |> fst |> neighbors
      |> List.iter ~f:(fun (neighbor, d) ->
             let tentative_g_score = g_score (fst current) + d in
             if tentative_g_score < g_score neighbor then (
               came_from := Map.update !came_from neighbor ~f:(Fn.const current);
               g_score_map :=
                 Map.update !g_score_map neighbor
                   ~f:(Fn.const tentative_g_score);
               let f_score = tentative_g_score + h neighbor in
               if not @@ Set.exists !open_set ~f:(fun (c, _) -> eq c neighbor)
               then open_set := Set.add !open_set (neighbor, f_score))))
  done;

  !result

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
  astar
    (module Node)
    (module Coords)
    ~start:(0, 0) ~goal:(max_x, max_y)
    ~h:(heuristic (max_x, max_y))
    ~eq:(fun (x1, y1) (x2, y2) -> x1 = x2 && y1 = y2)
    ~neighbors:(neighbors max_x max_y)
  |> Option.value_exn
  |> (fun (_, cost) -> cost)
  |> printf
       "The lowest total risk of any path from the top left to the bottom \
        right is %d\n";

  let max_x = (cave_width * 5) - 1 and max_y = (cave_height * 5) - 1 in
  astar
    (module Node)
    (module Coords)
    ~start:(0, 0) ~goal:(max_x, max_y)
    ~h:(heuristic (max_x, max_y))
    ~eq:(fun (x1, y1) (x2, y2) -> x1 = x2 && y1 = y2)
    ~neighbors:(neighbors max_x max_y)
  |> Option.value_exn
  |> (fun (_, cost) -> cost)
  |> printf
       "Using the full map, the lowest total risk of any path from the top \
        left to the bottom right is %d\n"
