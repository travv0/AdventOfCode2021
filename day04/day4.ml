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

let checkForWin (board : board) =
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

let markCell number board =
  board
  |> Array.map ~f:(fun row ->
         Array.map row ~f:(fun cell ->
             { cell with called = cell.called || cell.num = number }))

let sumUnmarked board =
  board
  |> Array.concat_map ~f:Fn.id
  |> Array.map ~f:(fun cell -> if not cell.called then cell.num else 0)
  |> Array.fold ~f:( + ) ~init:0

let calcWinningScore lastCalled board = sumUnmarked board * lastCalled

let rec findWinningScore lastCalled queue boards =
  match List.find ~f:checkForWin boards with
  | Some board -> calcWinningScore lastCalled board
  | None -> (
      match queue with
      | calledNum :: newQueue ->
          findWinningScore calledNum newQueue
            (List.map ~f:(markCell calledNum) boards)
      | [] -> failwith "game ended without a winner")

let findFirstWinningScore queue boards = findWinningScore 0 queue boards

let file_name =
  match Sys.get_argv () |> Array.to_list with
  | _ :: fn :: _ -> fn
  | _ -> "input.txt"

let { queue; boards } = In_channel.read_all file_name |> parse_input

let findLastWinningScore queue boards =
  let rec findLastWinningScore' queue boards =
    let newBoards = boards |> List.filter ~f:(not << checkForWin) in
    match (newBoards, queue) with
    | [ board ], calledNum :: newQueue ->
        findWinningScore calledNum newQueue [ markCell calledNum board ]
    | [], _ -> failwith "ran out of boards"
    | _, calledNum :: newQueue ->
        List.map ~f:(markCell calledNum) newBoards
        |> findLastWinningScore' newQueue
    | _, [] -> failwith "game ended without a winner"
  in
  findLastWinningScore' queue boards

let () =
  findFirstWinningScore queue boards
  |> printf "Score of first card to win: %d\n";

  findLastWinningScore queue boards |> printf "Score of last card to win: %d\n"
