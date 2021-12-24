open Base
open Stdio
open Printf

let ( >> ) f g x = g (f x)

type coords = { x : int; y : int }
type line = { start : coords; end_ : coords }

let file_name =
  match Sys.get_argv () |> Array.to_list with
  | _ :: fn :: _ -> fn
  | _ -> "input.txt"

let input = In_channel.read_all file_name

let parse_coords (coords : string) =
  match coords |> String.split ~on:',' with
  | [ x; y ] -> { x = Int.of_string x; y = Int.of_string y }
  | _ -> ksprintf failwith "bad parse for coords `%s`" coords

let parse_input (input : string) =
  input
  |> String.split_lines
  |> List.map ~f:(fun line ->
         match line |> Str.split (Str.regexp " -> ") with
         | [ start; end_ ] ->
             { start = parse_coords start; end_ = parse_coords end_ }
         | _ -> ksprintf failwith "bad parse for line `%s`" line)

let lines = parse_input input

let fill_line line =
  let sort (a, b) = if a < b then (a, b) else (b, a) in

  if line.start.x = line.end_.x then
    let y1, y2 = sort (line.start.y, line.end_.y) in

    List.range ~stop:`inclusive y1 y2
    |> List.map ~f:(fun y -> { x = line.start.x; y })
  else if line.start.y = line.end_.y then
    let x1, x2 = sort (line.start.x, line.end_.x) in

    List.range ~stop:`inclusive x1 x2
    |> List.map ~f:(fun x -> { x; y = line.start.y })
  else
    let xs =
      List.range ~stop:`inclusive
        ~stride:(if line.start.x < line.end_.x then 1 else -1)
        line.start.x line.end_.x
    in
    let ys =
      List.range ~stop:`inclusive
        ~stride:(if line.start.y < line.end_.y then 1 else -1)
        line.start.y line.end_.y
    in

    List.map2_exn xs ys ~f:(fun x y -> { x; y })

let is_horizontal_or_vertical line =
  line.start.x = line.end_.x || line.start.y = line.end_.y

let calc_overlaps lines =
  let coords =
    Sequence.(
      lines |> of_list |> map ~f:(fill_line >> of_list) |> concat |> to_list)
  in
  List.(
    coords
    |> sort ~compare:(fun a b ->
           String.compare (sprintf "%d,%d" a.x a.y) (sprintf "%d,%d" b.x b.y))
    |> group ~break:(fun a b -> a.x <> b.x || a.y <> b.y)
    |> map ~f:(count ~f:(Fn.const true))
    |> count ~f:(( < ) 1))

let () =
  lines
  |> List.filter ~f:is_horizontal_or_vertical
  |> calc_overlaps
  |> printf
       "Number of points where at least two horizontal or vertical lines \
        overlap: %d\n";

  lines
  |> calc_overlaps
  |> printf "Number of points where at least two lines overlap: %d\n"
