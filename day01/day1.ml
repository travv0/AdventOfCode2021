open Base
open Stdio

let file_name =
  match Sys.get_argv () |> Array.to_list with
  | _ :: fn :: _ -> fn
  | _ -> "input.txt"

let lines = In_channel.read_lines file_name |> List.map ~f:Int.of_string

let count_increases = function
  | x :: xs ->
      let prev = ref x in
      xs
      |> List.map ~f:(fun x ->
             let result = x > !prev in
             prev := x;
             result)
      |> List.count ~f:Fn.id
  | _ -> 0

let tails l =
  let rec tails' l acc =
    match l with [] -> acc |> List.rev | _ :: rest -> tails' rest (l :: acc)
  in
  tails' l []

let sum_group = function a :: b :: c :: _ -> Some (a + b + c) | _ -> None
let sums = lines |> tails |> List.filter_map ~f:sum_group

let () =
  count_increases lines |> printf "Number of times depth increases: %d\n";

  count_increases sums
  |> printf "Number of times sum in sliding window increases: %d\n"
