open Base
open Stdio

type fish = int64
type fishes = fish list

module Fish = Int64

let ( >> ) f g x = g (f x)
let ( << ) f g x = f (g x)

module List = struct
  include Base.List

  let adjust_at ~index ~f list =
    match List.split_n list index with
    | head, elem :: tail -> head @ (f elem :: tail)
    | _ -> list

  let count_by ~f ~compare ~break list =
    let compare_arg = compare in
    List.(
      list
      |> map ~f
      |> sort ~compare:compare_arg
      |> group ~break
      |> map ~f:(fun a -> (List.hd_exn a, length a)))
end

let parse_input (input : string) : fishes =
  let open Fish in
  let fish_nums = input |> String.strip |> String.split ~on:',' in
  let counts =
    List.count_by ~f:of_string
      ~compare:(fun a b -> a - b |> to_int_trunc)
      ~break:( <> ) fish_nums
  in

  List.range' ~stop:`inclusive ~stride:(( + ) 1L) ~compare 0L 8L
  |> List.map ~f:(fun i ->
         counts
         |> List.find ~f:(fst >> ( = ) i)
         |> Option.map ~f:snd
         |> Option.value ~default:0
         |> of_int)

let step (fishes : fishes) : fishes =
  let open Fish in
  match fishes with
  | zeros :: fishes ->
      List.adjust_at ~index:6 ~f:(( + ) zeros) fishes @ [ zeros ]
  | _ -> failwith "where your fishes at?"

let rec step_times (n : int) (fishes : fishes) : fishes =
  match n with 0 -> fishes | n -> step_times (n - 1) (step fishes)

let file_name =
  match Sys.get_argv () |> Array.to_list with
  | _ :: fn :: _ -> fn
  | _ -> "input.txt"

let fishes = In_channel.read_all file_name |> parse_input

let () =
  let open Fish in
  step_times 80 fishes
  |> List.fold ~init:zero ~f:( + )
  |> printf "Number of lanternfish after 80 days: %Ld\n";

  step_times 256 fishes
  |> List.fold ~init:zero ~f:( + )
  |> printf "Number of lanternfish after 256 days: %Ld\n"
