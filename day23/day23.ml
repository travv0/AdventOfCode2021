open Base
open Stdio
open Printf

let file_name =
  match Sys.get_argv () |> Array.to_list with
  | _ :: fn :: _ -> fn
  | _ -> "input.txt"

let input = In_channel.read_all file_name

type amphipod_type = Amber | Bronze | Copper | Desert
[@@deriving compare, sexp_of, equal]

type amphipod_state = Idle | Moving | Stopped | Goal
[@@deriving compare, sexp_of, equal]

type amphipod =
  { x : int
  ; y : int
  ; type_ : amphipod_type
  ; energy : int
  ; state : amphipod_state
  }
[@@deriving compare, sexp_of, equal]

type cell = Wall | Hallway | Room of amphipod_type
[@@deriving compare, sexp_of, equal]

module Coords = struct
  module T = struct
    type t = int * int [@@deriving compare, sexp_of]
  end

  include Comparator.Make (T)
  include T
end

module State = struct
  module T = struct
    type t = { burrow : cell array array; amphipods : amphipod list }
    [@@deriving compare, sexp_of, equal]
  end

  include Comparator.Make (T)
  include T

  let heuristic (state1 : t) (state2 : t) : int = failwith "unimplemented"
  let neighbors (state : t) : (t * int) list = failwith "unimplemented"

  let parse s : t =
    let int_to_amphipod_room = function
      | 3 -> Room Amber
      | 5 -> Room Bronze
      | 7 -> Room Copper
      | 9 -> Room Desert
      | x -> failwithf "bad x coord for amphipod to start at: %d" x ()
    in
    let burrow, amphipods =
      s
      |> String.split_lines
      |> List.foldi ~init:([], []) ~f:(fun y (burrow, amphipods) line ->
             let row, new_amphipods =
               line
               |> String.to_list
               |> List.foldi ~init:([], []) ~f:(fun x (row, amphipods) ->
                    function
                    | '#' | ' ' -> (Wall :: row, amphipods)
                    | '.' -> (Hallway :: row, amphipods)
                    | 'A' ->
                        ( int_to_amphipod_room x :: row
                        , { x; y; type_ = Amber; energy = 1; state = Idle }
                          :: amphipods )
                    | 'B' ->
                        ( int_to_amphipod_room x :: row
                        , { x; y; type_ = Bronze; energy = 10; state = Idle }
                          :: amphipods )
                    | 'C' ->
                        ( int_to_amphipod_room x :: row
                        , { x; y; type_ = Copper; energy = 100; state = Idle }
                          :: amphipods )
                    | 'D' ->
                        ( int_to_amphipod_room x :: row
                        , { x; y; type_ = Desert; energy = 1000; state = Idle }
                          :: amphipods )
                    | c -> failwithf "bad parse: %c" c ())
             in
             let row = row |> List.rev |> List.to_array in
             (row :: burrow, amphipods @ new_amphipods))
    in
    { burrow = burrow |> List.rev |> List.to_array; amphipods }
end

let start_state = State.parse input

let goal_state =
  State.parse
    "#############\n#...........#\n###A#B#C#D###\n  #A#B#C#D#\n  #########"
(*
   let () =
     Astar.path (module State) start_state goal_state
     |> Option.value_exn
     |> snd
     |> printf "The amphipods can be organized using a minimum of %d energy\n%!" *)
