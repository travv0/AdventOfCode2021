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

type amphipod = { type_ : amphipod_type; energy : int; state : amphipod_state }
[@@deriving compare, sexp_of, equal]

type open_space = Hallway | Room of amphipod_type
[@@deriving compare, sexp_of, equal]

type cell = Wall | Open of open_space | Amphipod of amphipod * open_space
[@@deriving compare, sexp_of, equal]

module Coords = struct
  module T = struct
    type t = int * int [@@deriving compare, sexp_of]
  end

  include Comparator.Make (T)
  include T
end

module State : sig
  include Astar.Elem

  val parse : string -> t
end = struct
  module T = struct
    type t = cell array array [@@deriving compare, sexp_of, equal]
  end

  include Comparator.Make (T)
  include T

  let heuristic (state1 : t) (state2 : t) : int = failwith "unimplemented"
  let neighbors (state : t) : (t * int) list = failwith "unimplemented"

  let parse s =
    let int_to_amphipod_room = function
      | 3 -> Room Amber
      | 5 -> Room Bronze
      | 7 -> Room Copper
      | 9 -> Room Desert
      | x -> failwithf "bad x coord for amphipod to start at: %d" x ()
    in
    s
    |> String.split_lines
    |> List.map ~f:(fun line ->
           line
           |> String.to_array
           |> Array.mapi ~f:(fun x -> function
                | '#' | ' ' -> Wall
                | '.' -> Open Hallway
                | 'A' ->
                    Amphipod
                      ( { type_ = Amber; energy = 1; state = Idle }
                      , int_to_amphipod_room x )
                | 'B' ->
                    Amphipod
                      ( { type_ = Bronze; energy = 10; state = Idle }
                      , int_to_amphipod_room x )
                | 'C' ->
                    Amphipod
                      ( { type_ = Copper; energy = 100; state = Idle }
                      , int_to_amphipod_room x )
                | 'D' ->
                    Amphipod
                      ( { type_ = Desert; energy = 1000; state = Idle }
                      , int_to_amphipod_room x )
                | c -> failwithf "bad parse: %c" c ()))
    |> List.to_array
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
