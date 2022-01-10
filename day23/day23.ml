open Base
open Stdio
open Printf

let ( << ) = Fn.compose

let file_name =
  match Sys.get_argv () |> Array.to_list with
  | _ :: fn :: _ -> fn
  | _ -> "input.txt"

let input = In_channel.read_all file_name

type amphipod_type = Amber | Bronze | Copper | Desert
[@@deriving compare, sexp_of, equal]

type amphipod_state = Idle | Moving | Stopped | MovingToGoal | Goal
[@@deriving compare, sexp_of, equal]

type amphipod =
  { type_ : amphipod_type
  ; x : int
  ; y : int
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

  let heuristic ({ amphipods; _ } : t) ({ amphipods = goal_amphipods; _ } : t) :
      int =
    let amphipods = List.sort amphipods ~compare:compare_amphipod in
    let goal_amphipods = List.sort goal_amphipods ~compare:compare_amphipod in
    List.zip_exn amphipods goal_amphipods
    |> List.map ~f:(fun ({ x; y; _ }, { x = goal_x; y = goal_y; _ }) ->
           abs (goal_x - x) + abs (goal_y - y))
    |> List.fold ~init:0 ~f:( + )

  let get_cell_xy burrow x y = try burrow.(y).(x) with _ -> Wall
  let get_cell burrow { x; y; _ } = get_cell_xy burrow x y

  let is_room burrow x y =
    try match burrow.(y).(x) with Room _ -> true | _ -> false
    with _ -> false

  let is_wall burrow x y =
    try match burrow.(y).(x) with Wall -> true | _ -> false with _ -> false

  let get_amphipod amphipods x y =
    List.find amphipods ~f:(fun { x = x2; y = y2; _ } -> x = x2 && y = y2)

  let print { burrow; amphipods } =
    Array.iteri burrow ~f:(fun y row ->
        Array.iteri row ~f:(fun x cell ->
            printf "%c"
            @@
            match get_amphipod amphipods x y with
            | Some { type_ = Amber; _ } -> 'A'
            | Some { type_ = Bronze; _ } -> 'B'
            | Some { type_ = Copper; _ } -> 'C'
            | Some { type_ = Desert; _ } -> 'D'
            | None -> ( match cell with Wall -> '#' | Hallway | Room _ -> '.'));
        printf "\n%!")

  let neighbors ({ burrow; amphipods } : t) : (t * int) list =
    if
      List.count amphipods ~f:(fun amphipod ->
          equal_amphipod_state amphipod.state Moving
          || equal_amphipod_state amphipod.state MovingToGoal)
      > 1
    then failwith "multiple amphipods moving at once";
    let get_open_neighbors amphipod amphipods =
      match amphipod.state with
      | Goal -> []
      | _ ->
          [ (0, -1); (1, 0); (0, 1); (-1, 0) ]
          |> List.filter_map ~f:(fun (dx, dy) ->
                 let new_x = amphipod.x + dx and new_y = amphipod.y + dy in
                 if
                   List.exists amphipods ~f:(fun { x; y; _ } ->
                       x = new_x && y = new_y)
                 then None
                 else
                   let new_state =
                     ( { burrow
                       ; amphipods =
                           { amphipod with
                             x = new_x
                           ; y = new_y
                           ; state =
                               (match get_cell_xy burrow new_x new_y with
                               | Room room_type
                                 when equal_amphipod_type amphipod.type_
                                        room_type
                                      && (is_wall burrow new_x (new_y + 1)
                                         || List.exists amphipods
                                              ~f:(fun { x; y; _ } ->
                                                x = new_x && y = new_y)) ->
                                   Goal
                               | _ -> (
                                   match amphipod.state with
                                   | Moving -> Moving
                                   | MovingToGoal -> MovingToGoal
                                   | Stopped -> MovingToGoal
                                   | Idle -> Moving
                                   | Goal ->
                                       failwith
                                         "trying to move amphipod from goal"))
                           }
                           :: amphipods
                       }
                     , amphipod.energy )
                   in
                   match get_cell_xy burrow new_x new_y with
                   | Wall -> None
                   | Hallway -> Some new_state
                   | Room room_type
                     when equal_amphipod_type amphipod.type_ room_type
                          && not
                               (List.exists amphipods
                                  ~f:(fun { type_; x; y; _ } ->
                                    x = new_x
                                    && y > new_y
                                    && not
                                         (equal_amphipod_type type_
                                            amphipod.type_))) ->
                       Some new_state
                   | Room _ -> None)
    in
    let partition_in_doorway () =
      match
        List.partition_tf amphipods ~f:(fun amphipod ->
            equal_cell (get_cell burrow amphipod) Hallway
            && is_room burrow amphipod.x (amphipod.y + 1))
      with
      | [ amphipod ], amphipods -> Some (amphipod, amphipods)
      | _ :: _, _ -> failwith "more than one amphipod in front of doorway"
      | _ -> None
    in
    let partition_moving_to_goal () =
      match
        List.partition_tf amphipods ~f:(fun { state; _ } ->
            equal_amphipod_state state MovingToGoal)
      with
      | [ amphipod ], amphipods -> Some (amphipod, amphipods)
      | _ :: _, _ -> failwith "more than one amphipod moving to goal"
      | _ -> None
    in
    match partition_in_doorway () with
    | Some (amphipod, amphipods) -> get_open_neighbors amphipod amphipods
    | None -> (
        match partition_moving_to_goal () with
        | Some (amphipod, amphipods) -> get_open_neighbors amphipod amphipods
        | None ->
            List.concat_map amphipods ~f:(fun amphipod ->
                List.filter amphipods ~f:(not << equal_amphipod amphipod)
                |> get_open_neighbors amphipod))

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

let () =
  Astar.path (module State) start_state goal_state
  |> Option.value_exn
  |> snd
  |> printf "The amphipods can be organized using a minimum of %d energy\n%!"
