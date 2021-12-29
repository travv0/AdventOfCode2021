open Base
open Stdio
open Printf

type coords = { x : int; y : int }

module Coords = struct
  module T = struct
    type t = coords

    let compare { x = x1; y = y1 } { x = x2; y = y2 } =
      List.compare Int.compare [ x1; y1 ] [ x2; y2 ]

    let sexp_of_t { x; y } = List.sexp_of_t Int.sexp_of_t [ x; y ]
  end

  include T
  include Comparator.Make (T)
end

type axis = X | Y
type paper = (coords, Coords.comparator_witness) Set.t

module Axis = struct
  let of_string = function
    | "X" | "x" -> X
    | "Y" | "y" -> Y
    | a -> failwithf "bad parse: %s" a ()
end

type fold = { axis : axis; coord : int }
type parse_result = { paper : paper; fold_queue : fold list }

let file_name =
  match Sys.get_argv () |> Array.to_list with
  | _ :: fn :: _ -> fn
  | _ -> "input.txt"

let input = In_channel.read_all file_name

let parse_coords s =
  match s |> String.split ~on:',' with
  | [ x; y ] -> { x = Int.of_string x; y = Int.of_string y }
  | _ -> failwithf "bad parse: %s" s ()

let parse_fold s =
  match
    s
    |> String.split ~on:' '
    |> List.map ~f:(fun s -> s |> String.split ~on:'=')
  with
  | [ _; _; [ axis; coord ] ] ->
      { axis = Axis.of_string axis; coord = Int.of_string coord }
  | _ -> failwithf "bad parse: %s" s ()

let parse_input input : parse_result =
  match input |> Str.split (Str.regexp "\r?\n\r?\n") with
  | [ coords; folds ] ->
      let paper =
        coords
        |> String.split_lines
        |> List.map ~f:parse_coords
        |> Set.of_list (module Coords)
      in
      let fold_queue = folds |> String.split_lines |> List.map ~f:parse_fold in
      { paper; fold_queue }
  | _ -> failwithf "bad parse: %s" input ()

let fold_horizontal fold_y (paper : paper) : paper =
  let were_dots, are_dots = Set.partition_tf paper ~f:(fun c -> c.y > fold_y) in
  were_dots
  |> Set.map
       (module Coords)
       ~f:(fun { x; y } -> { x; y = fold_y - (y - fold_y) })
  |> Set.union are_dots

let fold_vertical fold_x (paper : paper) : paper =
  let were_dots, are_dots = Set.partition_tf paper ~f:(fun c -> c.x > fold_x) in
  were_dots
  |> Set.map
       (module Coords)
       ~f:(fun { x; y } -> { x = fold_x - (x - fold_x); y })
  |> Set.union are_dots

let print_paper paper =
  let xs, ys =
    paper |> Set.to_list |> List.map ~f:(fun { x; y } -> (x, y)) |> List.unzip
  in
  let max_x = List.max_elt xs ~compare:Int.compare |> Option.value_exn
  and max_y = List.max_elt ys ~compare:Int.compare |> Option.value_exn in
  for y = 0 to max_y do
    for x = 0 to max_x do
      if Set.exists paper ~f:(fun { x = cx; y = cy } -> cx = x && cy = y) then
        printf "#"
      else printf "."
    done;
    printf "\n"
  done

let { paper; fold_queue } = parse_input input

let rec fold_paper fold_queue paper =
  match fold_queue with
  | { axis; coord } :: fold_queue ->
      let fold_fn =
        match axis with X -> fold_vertical | Y -> fold_horizontal
      in
      fold_paper fold_queue (fold_fn coord paper)
  | _ -> paper

let () =
  paper
  |> fold_paper [ List.hd_exn fold_queue ]
  |> Set.length
  |> printf "After the first fold, only %d dots are visible\n";

  printf "The code to activate the infrared thermal imaging camera system is\n";
  paper |> fold_paper fold_queue |> print_paper
