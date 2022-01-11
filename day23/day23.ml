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
    List.zip_exn amphipods goal_amphipods
    (* |> List.concat_map ~f:(fun (amps, goal_amps) ->
           let goal_x = (List.hd_exn goal_amps).x in
           List.zip_exn
             (List.sort amps ~compare:(fun amp1 amp2 ->
                  Int.compare (amp1.x - goal_x) (amp2.x - goal_x)))
             goal_amps) *)
    |> List.map
         ~f:(fun
              ( { x; y; energy; type_; _ }
              , { x = goal_x; y = goal_y; type_ = goal_type; _ } )
            ->
           (* sexp_of_amphipod amp |> print_s;
              sexp_of_amphipod goal_amp |> print_s;
              printf "\n%!"; *)
           if not @@ equal_amphipod_type type_ goal_type then
             failwith "mismatched types";
           let vertical = if x = goal_x then 0 else y - 1 in
           (vertical + abs (goal_x - x) + abs (goal_y - y)) * energy)
    |> List.fold ~init:0 ~f:( + )

  let equal { amphipods = amphipods1; _ } { amphipods = amphipods2; _ } =
    List.equal
      (fun { x = x1; y = y1; type_ = type1; _ }
           { x = x2; y = y2; type_ = type2; _ } ->
        x1 = x2 && y1 = y2 && equal_amphipod_type type1 type2)
      amphipods1 amphipods2

  let get_cell_xy burrow x y = try burrow.(y).(x) with _ -> Wall
  let get_cell burrow { x; y; _ } = get_cell_xy burrow x y

  let is_room burrow x y =
    match get_cell_xy burrow x y with Room _ -> true | _ -> false

  let is_wall burrow x y =
    match get_cell_xy burrow x y with Wall -> true | _ -> false

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
        printf "\n");
    printf "\n%!"

  let amphipod_has_state (amphipods : amphipod list) state =
    List.exists amphipods ~f:(fun amphipod ->
        equal_amphipod_state state amphipod.state)

  let stop_amphipods (amphipods : amphipod list) =
    List.map amphipods ~f:(fun amphipod ->
        { amphipod with
          state =
            (match amphipod.state with
            | (Idle | Goal | Stopped) as s -> s
            | Moving -> Stopped
            | MovingToGoal ->
                failwith "trying to stop amphipod from moving to goal")
        })

  let get_open_neighbors burrow amphipod amphipods =
    match amphipod.state with
    | Goal -> []
    | _ ->
        if amphipod_has_state amphipods MovingToGoal then []
        else
          [ (0, -1); (1, 0); (0, 1); (-1, 0) ]
          |> List.filter_map ~f:(fun (dx, dy) ->
                 let new_x = amphipod.x + dx and new_y = amphipod.y + dy in
                 if
                   List.exists amphipods ~f:(fun { x; y; _ } ->
                       x = new_x && y = new_y)
                 then None
                 else
                   let new_amphipod =
                     { amphipod with
                       x = new_x
                     ; y = new_y
                     ; state =
                         (match get_cell_xy burrow new_x new_y with
                         | Room room_type
                           when equal_amphipod_type amphipod.type_ room_type
                                && (is_wall burrow new_x (new_y + 1)
                                   || List.exists amphipods
                                        ~f:(fun { x; y; _ } ->
                                          x = new_x && y = new_y + 1)) ->
                             Goal
                         | _ -> (
                             match amphipod.state with
                             | Moving -> Moving
                             | MovingToGoal -> MovingToGoal
                             | Stopped -> MovingToGoal
                             | Idle -> Moving
                             | Goal ->
                                 failwith "trying to move amphipod from goal"))
                     }
                   in
                   let new_state =
                     ( { burrow
                       ; amphipods =
                           new_amphipod :: stop_amphipods amphipods
                           |> List.sort ~compare:compare_amphipod
                       }
                     , amphipod.energy )
                   in
                   match get_cell_xy burrow new_x new_y with
                   | Wall -> None
                   | Hallway -> Some new_state
                   | Room room_type
                     when new_y < amphipod.y
                          || equal_amphipod_type amphipod.type_ room_type
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

  let set_goal_states burrow amphipods : amphipod list =
    List.map amphipods ~f:(fun amphipod ->
        { amphipod with
          state =
            (match get_cell burrow amphipod with
            | Room room_type
              when equal_amphipod_type amphipod.type_ room_type
                   && (is_wall burrow amphipod.x (amphipod.y + 1)
                      || List.exists amphipods ~f:(fun { x; y; type_; _ } ->
                             x = amphipod.x
                             && y = amphipod.y + 1
                             && equal_amphipod_type type_ amphipod.type_)) ->
                Goal
            | _ -> amphipod.state)
        })

  let neighbors ({ burrow; amphipods } : t) : (t * int) list =
    (* print { burrow; amphipods }; *)
    (* check for invalid states *)
    if
      List.count amphipods ~f:(fun amphipod ->
          equal_amphipod_state amphipod.state Moving
          || equal_amphipod_state amphipod.state MovingToGoal)
      > 1
    then failwith "multiple amphipods moving at once";
    if
      List.exists amphipods ~f:(fun amphipod ->
          match amphipod.state with
          | Idle | Moving -> false
          | _ -> (
              match get_cell burrow amphipod with
              | Room room_type ->
                  (not (equal_amphipod_type room_type amphipod.type_))
                  && amphipod.y = Array.length burrow - 2
              | _ -> false))
    then failwith "amphipod entered someone else's room";
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
    | Some (amphipod, amphipods) -> get_open_neighbors burrow amphipod amphipods
    | None -> (
        match partition_moving_to_goal () with
        | Some (amphipod, amphipods) ->
            get_open_neighbors burrow amphipod amphipods
        | None ->
            List.concat_map amphipods ~f:(fun amphipod ->
                List.filter amphipods ~f:(not << equal_amphipod amphipod)
                |> get_open_neighbors burrow amphipod))

  let parse ?(hidden_lines = "") s : t =
    let int_to_amphipod_room = function
      | 3 -> Room Amber
      | 5 -> Room Bronze
      | 7 -> Room Copper
      | 9 -> Room Desert
      | x -> failwithf "bad x coord for amphipod to start at: %d" x ()
    in
    let unfold lines =
      let top, bottom = List.split_n lines 3 in
      top @ String.split_lines hidden_lines @ bottom
    in
    let burrow, amphipods =
      s
      |> String.split_lines
      |> unfold
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
    let burrow = burrow |> List.rev |> List.to_array in
    let amphipods =
      amphipods |> set_goal_states burrow |> List.sort ~compare:compare_amphipod
    in
    { burrow; amphipods }
end

let print_path = true

let () =
  let start_state = State.parse input in

  let goal_state =
    State.parse
      "#############\n#...........#\n###A#B#C#D###\n  #A#B#C#D#\n  #########"
  in

  Astar.path (module State) start_state goal_state |> Option.value_exn
  |> fun (path, cost) ->
  printf "The amphipods can be organized using a minimum of %d energy\n%!" cost;
  if print_path then List.iter path ~f:State.print

let () =
  let start_state =
    State.parse input ~hidden_lines:"  #D#C#B#A#\n  #D#B#A#C#"
  in

  let goal_state =
    State.parse
      "#############\n\
       #...........#\n\
       ###A#B#C#D###\n\
      \  #A#B#C#D#\n\
      \  #A#B#C#D#\n\
      \  #A#B#C#D#\n\
      \  #########"
  in

  Astar.path (module State) start_state goal_state |> Option.value_exn
  |> fun (path, cost) ->
  printf
    "With the full diagram, the amphipods can be organized using a minimum of \
     %d energy\n\
     %!"
    cost;
  if print_path then List.iter path ~f:State.print
