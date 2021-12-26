open Base
open Stdio
open Printf

let file_name =
  match Sys.get_argv () |> Array.to_list with
  | _ :: fn :: _ -> fn
  | _ -> "input.txt"

let input = In_channel.read_all file_name

module CharList = struct
  module T = struct
    type t = char list

    let compare = List.compare Char.compare
    let sexp_of_t = List.sexp_of_t Char.sexp_of_t
  end

  include T
  include Comparator.Make (T)
end

type parse_result = {
  polymer : char list;
  rules : (char list, char, CharList.comparator_witness) Map.t;
}

let parse_input input : parse_result =
  match Str.split (Str.regexp "\r?\n\r?\n") input with
  | [ polymer; rules ] ->
      let polymer = String.to_list polymer in
      let rules =
        rules |> String.split_lines
        |> List.map ~f:(fun line ->
               match Str.split (Str.regexp " -> ") line with
               | [ from; to_ ] -> (String.to_list from, to_.[0])
               | _ -> failwithf "bad parse: %s" line ())
        |> Map.of_alist_exn (module CharList)
      in
      { polymer; rules }
  | _ -> failwithf "bad parse: %s" input ()

let tails l =
  let rec tails' l acc =
    match l with [] -> acc |> List.rev | _ :: rest -> tails' rest (l :: acc)
  in
  tails' l []

let first_insertion rules = function
  | c1 :: c2 :: _ -> Map.find rules [ c1; c2 ]
  | _ -> None

let step rules polymer =
  let insertions = polymer |> tails |> List.map ~f:(first_insertion rules) in
  List.map2_exn polymer insertions ~f:(fun c1 c2 -> [ Some c1; c2 ])
  |> List.concat |> List.filter_opt

let rec step_times n rules polymer =
  if n < 1 then polymer else step_times (n - 1) rules (step rules polymer)

let { rules; polymer } = parse_input input

let subtract_least_common_from_most_common polymer =
  let sorted =
    polymer
    |> List.sort ~compare:Char.compare
    |> List.group ~break:Char.( <> )
    |> List.sort ~compare:(fun a b -> List.length a - List.length b)
  in
  let least = List.hd_exn sorted and most = List.last_exn sorted in
  List.length most - List.length least

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
