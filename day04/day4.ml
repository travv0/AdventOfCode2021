open Base
open Stdio
open Printf

let ( >> ) f g x = g (f x)
let ( << ) f g x = f (g x)

type cell = { num : int; called : bool }
type board = cell array array
type parse_result = { queue : int list; boards : board list }

let parse_input (input : string) : parse_result =
  let sections = input |> String.strip |> Str.split (Str.regexp "\r?\n\r?\n") in
  match sections with
  | first :: rest ->
      let queue = first |> String.split ~on:',' |> List.map ~f:Int.of_string in
      let boards =
        rest
        |> List.map ~f:(fun board ->
               let lines = board |> Str.split (Str.regexp "\r?\n") in
               lines
               |> List.map ~f:(fun line ->
                      line
                      |> String.split ~on:' '
                      |> List.filter ~f:(not << String.equal "")
                      |> List.map ~f:(fun s ->
                             { num = Int.of_string s; called = false })
                      |> List.to_array)
               |> List.to_array)
      in
      { queue; boards }
  | _ -> ksprintf failwith "bad parse: %s" input

let check_for_win (board : board) =
  let range = List.range 0 (Array.length board.(0)) in
  match Array.transpose board with
  | Some transposed ->
      range
      |> List.exists ~f:(fun i ->
             board.(i) |> Array.for_all ~f:(fun cell -> cell.called))
      || range
         |> List.exists ~f:(fun i ->
                transposed.(i) |> Array.for_all ~f:(fun cell -> cell.called))
  | _ -> ksprintf failwith "board not proper matrix"

let mark_cell number board =
  board
  |> Array.map ~f:(fun row ->
         Array.map row ~f:(fun cell ->
             { cell with called = cell.called || cell.num = number }))

let sum_unmarked board =
  board
  |> Array.concat_map ~f:Fn.id
  |> Array.map ~f:(fun cell -> if not cell.called then cell.num else 0)
  |> Array.fold ~f:( + ) ~init:0

let calc_winning_score last_called board = sum_unmarked board * last_called

let rec find_winning_score last_called queue boards =
  match List.find ~f:check_for_win boards with
  | Some board -> calc_winning_score last_called board
  | None -> (
      match queue with
      | called_num :: new_queue ->
          find_winning_score called_num new_queue
            (List.map ~f:(mark_cell called_num) boards)
      | [] -> failwith "game ended without a winner")

let find_first_winning_score queue boards = find_winning_score 0 queue boards

let file_name =
  match Sys.get_argv () |> Array.to_list with
  | _ :: fn :: _ -> fn
  | _ -> "input.txt"

let { queue; boards } = In_channel.read_all file_name |> parse_input

let find_last_winning_score queue boards =
  let rec find_last_winning_score' queue boards =
    let new_boards = boards |> List.filter ~f:(not << check_for_win) in
    match (new_boards, queue) with
    | [ board ], called_num :: new_queue ->
        find_winning_score called_num new_queue [ mark_cell called_num board ]
    | _, called_num :: new_queue ->
        List.map ~f:(mark_cell called_num) new_boards
        |> find_last_winning_score' new_queue
    | _, [] -> failwith "game ended without a winner"
  in
  find_last_winning_score' queue boards

let () =
  find_first_winning_score queue boards
  |> printf "Score of first card to win: %d\n";

  find_last_winning_score queue boards
  |> printf "Score of last card to win: %d\n"
