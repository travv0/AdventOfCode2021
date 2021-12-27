open Base
open Stdio
open Printf

let file_name =
  match Sys.get_argv () |> Array.to_list with
  | _ :: fn :: _ -> fn
  | _ -> "input.txt"

let input = In_channel.read_all file_name

type polymer = (string, int, String.comparator_witness) Map.t
type rules = (string, char, String.comparator_witness) Map.t
type parse_result = { polymer : polymer; rules : rules }

let tails l =
  let rec tails' l acc =
    match l with [] -> acc |> List.rev | _ :: rest -> tails' rest (l :: acc)
  in
  tails' l []

let make_polymer s =
  s |> String.to_list |> tails
  |> List.filter_map ~f:(function
       | a :: b :: _ -> Some (String.of_char_list [ a; b ], 1)
       | a :: _ -> Some (String.of_char_list [ a; ' ' ], 1)
       | _ -> None)
  |> Map.of_alist_fold (module String) ~init:0 ~f:( + )

let parse_input input : parse_result =
  match Str.split (Str.regexp "\r?\n\r?\n") input with
  | [ polymer; rules ] ->
      let polymer = make_polymer polymer in
      let rules =
        rules |> String.split_lines
        |> List.map ~f:(fun line ->
               match Str.split (Str.regexp " -> ") line with
               | [ from; to_ ] -> (from, to_.[0])
               | _ -> failwithf "bad parse: %s" line ())
        |> Map.of_alist_exn (module String)
      in
      { polymer; rules }
  | _ -> failwithf "bad parse: %s" input ()

let update_polymer rules (k, v) =
  match Map.find rules k with
  | Some insertion ->
      [ [ k.[0]; insertion ]; [ insertion; k.[1] ] ]
      |> List.map ~f:(fun l -> (String.of_char_list l, v))
  | None -> [ (k, v) ]

let step (rules : rules) polymer : polymer =
  polymer |> Map.to_alist
  |> List.concat_map ~f:(update_polymer rules)
  |> Map.of_alist_fold (module String) ~init:0 ~f:( + )

let rec step_times n (rules : rules) polymer =
  if n < 1 then polymer else step_times (n - 1) rules (step rules polymer)

let { rules; polymer } = parse_input input

let subtract_least_common_from_most_common polymer =
  let open List in
  let sorted =
    polymer |> Map.to_alist
    |> map ~f:(fun (a, b) -> (a.[0], b))
    |> Map.of_alist_fold (module Char) ~init:0 ~f:( + )
    |> Map.to_alist
    |> sort ~compare:(fun (_, b1) (_, b2) -> b1 - b2)
  in
  let least = hd_exn sorted and most = last_exn sorted in
  snd most - snd least

let () =
  step_times 10 rules polymer
  |> subtract_least_common_from_most_common
  |> printf
       "After 10 steps, the quantity of the least common element subtracted \
        from the quantity of the most common element is %d\n";

  step_times 40 rules polymer
  |> subtract_least_common_from_most_common
  |> printf
       "After 40 steps, the quantity of the least common element subtracted \
        from the quantity of the most common element is %d\n"
