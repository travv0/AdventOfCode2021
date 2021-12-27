open Base
open Stdio
open Printf

let file_name =
  match Sys.get_argv () |> Array.to_list with
  | _ :: fn :: _ -> fn
  | _ -> "input.txt"

let input = In_channel.read_all file_name

module Node = struct
  module T = struct
    type t = (int * int) * int

    let compare ((x1, y1), w1) ((x2, y2), w2) =
      List.compare Int.compare [ x1; y1; w1 ] [ x2; y2; w2 ]

    let sexp_of_t ((x, y), w) =
      Sexp.List [ List.sexp_of_t Int.sexp_of_t [ x; y ]; Int.sexp_of_t w ]
  end

  include T
  include Comparator.Make (T)
end

let uniform_cost_search comparator ~start ~goal ~eq ~neighbors =
  let node = ref (start, 0) in
  let goal = (goal, Int.max_value) in
  let frontier = ref @@ Set.singleton comparator !node in
  let explored = ref @@ Set.empty comparator in
  let rec loop () =
    if Set.is_empty !frontier then None
    else (
      node := Set.min_elt_exn !frontier;
      frontier := Set.remove_index !frontier 0;
      if eq !node goal then Some !node
      else (
        explored := Set.add !explored !node;
        neighbors !node
        |> List.iter ~f:(fun (n, cost) ->
               if
                 (not (Set.exists !explored ~f:(eq (n, cost))))
                 && not (Set.exists !frontier ~f:(eq (n, cost)))
               then frontier := Set.add !frontier (n, cost)
               else
                 match Set.find !frontier ~f:(eq (n, cost)) with
                 | Some (a, frontier_cost) when frontier_cost > cost ->
                     frontier :=
                       Set.add
                         (Set.remove !frontier (a, frontier_cost))
                         (n, cost)
                 | _ -> ());
        loop ()))
  in
  loop ()

let parse_input input =
  input |> String.split_lines
  |> List.map ~f:(fun line ->
         line |> String.to_array
         |> Array.map ~f:(fun c -> sprintf "%c" c |> Int.of_string))
  |> List.to_array

let cave = parse_input input

let neighbors ((x, y), w) =
  [ (-1, 0); (0, -1); (1, 0); (0, 1) ]
  |> List.filter_map ~f:(fun (dx, dy) ->
         try Some ((x + dx, y + dy), cave.(x + dx).(y + dy) + w)
         with _ -> None)

let () =
  let max_x = Array.length cave - 1 and max_y = Array.length cave.(0) - 1 in
  uniform_cost_search
    (module Node)
    ~start:(0, 0) ~goal:(max_x, max_y)
    ~eq:(fun ((x1, y1), _) ((x2, y2), _) -> x1 = x2 && y1 = y2)
    ~neighbors
  |> Option.value_exn |> snd
  |> printf
       "The lowest total risk of any path from the top left to the bottom \
        right is %d\n"
