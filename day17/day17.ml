open Base
open Stdio
open Printf

let file_name =
  match Sys.get_argv () |> Array.to_list with
  | _ :: fn :: _ -> fn
  | _ -> "input.txt"

let input = In_channel.read_all file_name

module Probe = struct
  type t = { x : int; y : int; x_velocity : int; y_velocity : int }

  let make x_velocity y_velocity = { x = 0; y = 0; x_velocity; y_velocity }

  let step probe =
    { x = probe.x + probe.x_velocity
    ; y = probe.y + probe.y_velocity
    ; x_velocity =
        (if probe.x_velocity > 0 then probe.x_velocity - 1
        else if probe.x_velocity < 0 then probe.x_velocity + 1
        else 0)
    ; y_velocity = probe.y_velocity - 1
    }

  let hits_target probe ~min_x ~max_x ~min_y ~max_y : int option =
    let rec hits_target' probe peak_y =
      if probe.y < min_y && probe.y_velocity < 0 then None
      else if
        min_x <= probe.x
        && probe.x <= max_x
        && min_y <= probe.y
        && probe.y <= max_y
      then Some peak_y
      else
        let next_probe = step probe in
        hits_target' next_probe (max peak_y next_probe.y)
    in
    hits_target' probe probe.y
end

type parse_result = { min_x : int; max_x : int; min_y : int; max_y : int }

let parse_input input : parse_result =
  match Re.matches (Re.compile (Re.Pcre.re "(-?[0-9]+)")) input with
  | [ min_x; max_x; min_y; max_y ] ->
      { min_x = Int.of_string min_x
      ; max_x = Int.of_string max_x
      ; min_y = Int.of_string min_y
      ; max_y = Int.of_string max_y
      }
  | _ -> failwithf "bad parse: %s" input ()

let { min_x; max_x; min_y; max_y } = parse_input input
let x_range = List.range 0 max_x ~stop:`inclusive

let y_range =
  let y = max (abs min_y) (abs max_y) in
  List.range (neg y) y

let hits =
  x_range
  |> List.concat_map ~f:(fun x ->
         y_range
         |> List.filter_map ~f:(fun y ->
                Probe.(make x y |> hits_target ~min_x ~max_x ~min_y ~max_y)))

let () =
  hits
  |> List.max_elt ~compare:Int.compare
  |> Option.value_exn
  |> printf "The highest y position the probe can reach is %d\n";

  hits
  |> List.length
  |> printf
       "There are %d distinct initial velocity values that cause the probe to \
        fall within the target area\n"
