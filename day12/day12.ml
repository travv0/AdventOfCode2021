open Base
open Printf
open Stdio

let file_name =
  match Sys.get_argv () |> Array.to_list with
  | _ :: fn :: _ -> fn
  | _ -> "input.txt"

let input = In_channel.read_all file_name

let parse_input input =
  let open List in
  input
  |> String.split_lines
  |> concat_map ~f:(fun line ->
         match String.split line ~on:'-' with
         | [ from; to_ ] -> [ (from, to_); (to_, from) ]
         | _ -> failwithf "bad line: %s" line ())
  |> sort ~compare:(fun (a, _) (b, _) -> String.compare a b)
  |> group ~break:(fun (a, _) (b, _) -> not @@ String.equal a b)
  |> fold ~init:[] ~f:(fun acc a ->
         let key = hd_exn a |> fst
         and value = map a ~f:snd |> Set.of_list (module String) in
         (key, value) :: acc)
  |> Map.of_alist_exn (module String)

let is_small_cave = String.for_all ~f:Char.is_lowercase

module List_of_string = struct
  module T = struct
    type t = string list [@@deriving compare, sexp_of]
  end

  include T
  include Comparator.Make (T)
end

module List = struct
  include List

  let distinct comparator l = Set.of_list comparator l |> Set.to_list
end

let walk_nodes (from : string) (to_ : string) part connections :
    string list list =
  let rec walk_nodes'
      from
      (dead_nodes : (string, _) Set.t)
      (path : string list)
      (two_time_queue : string option) : string list list =
    if String.equal from to_ then [ List.rev @@ (to_ :: path) ]
    else
      let valid_children =
        Map.find connections from
        |> Option.map ~f:(fun c -> Set.diff c dead_nodes)
      in
      match valid_children with
      | None -> []
      | Some children when Set.is_empty children -> []
      | Some children ->
          let new_dead_nodes, new_two_time_queue =
            if is_small_cave from then
              match two_time_queue with
              | Some cave when String.equal cave from -> (dead_nodes, None)
              | Some cave -> (Set.add dead_nodes from, Some cave)
              | None -> (Set.add dead_nodes from, None)
            else (dead_nodes, two_time_queue)
          in
          children
          |> Set.to_list
          |> List.concat_map ~f:(fun c ->
                 walk_nodes' c new_dead_nodes (from :: path) new_two_time_queue)
  in
  match part with
  | `part1 -> walk_nodes' from (Set.empty (module String)) [] None
  | `part2 ->
      let small_caves =
        Map.keys connections
        |> List.filter
             ~f:
               String.(
                 fun cave -> is_small_cave cave && cave <> from && cave <> to_)
      in
      small_caves
      |> List.concat_map ~f:(fun cave ->
             Some cave |> walk_nodes' from (Set.empty (module String)) [])
      |> List.distinct (module List_of_string)

let () =
  let connections = input |> parse_input in

  connections
  |> walk_nodes "start" "end" `part1
  |> List.length
  |> printf
       "There are %d paths through the cave system if each small cave is \
        visited once.\n";

  connections
  |> walk_nodes "start" "end" `part2
  |> List.length
  |> printf
       "There are %d paths through the cave system if one small cave can be \
        visited twice.\n"
