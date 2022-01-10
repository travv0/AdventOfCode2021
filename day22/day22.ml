open Base
open Stdio
open Printf

let file_name =
  match Sys.get_argv () |> Array.to_list with
  | _ :: fn :: _ -> fn
  | _ -> "input.txt"

let input = In_channel.read_all file_name

module Cube = struct
  module T = struct
    type t = { x : int; y : int; z : int } [@@deriving compare, sexp_of]
  end

  include T
  include Comparator.Make (T)
end

type cuboid = (Cube.t, Cube.comparator_witness) Set.t
type power = On | Off

module Power = struct
  let parse = function
    | "on" -> On
    | "off" -> Off
    | s -> failwithf "bad parse: %s" s ()
end

module Reactor = struct
  type t = (Cube.t, Cube.comparator_witness) Set.t

  let set_cuboid reactor ~to_ ~cuboid =
    match to_ with
    | On -> Set.union reactor cuboid
    | Off -> Set.diff reactor cuboid

  let run_steps reactor steps =
    List.fold steps ~init:reactor ~f:(fun reactor (to_, cuboid) ->
        set_cuboid reactor ~to_ ~cuboid)
end

module CuboidRange = struct
  type range = { start : int; stop : int }
  type t = { x : range; y : range; z : range }

  let parse s : t =
    let parse_range s =
      match String.lsplit2 s ~on:'=' with
      | Some (_, r) -> (
          match Str.split (Str.regexp "\.\.") r with
          | [ beginning; ending ] ->
              { start = Int.of_string beginning; stop = Int.of_string ending }
          | _ -> failwithf "bad parse: %s" r ())
      | _ -> failwithf "bad parse: %s" s ()
    in
    match String.split s ~on:',' with
    | [ x; y; z ] -> { x = parse_range x; y = parse_range y; z = parse_range z }
    | _ -> failwithf "bad parse: %s" s ()

  let to_cuboid range : cuboid =
    List.range ~stop:`inclusive range.x.start range.x.stop
    |> List.concat_map ~f:(fun x ->
           List.range ~stop:`inclusive range.y.start range.y.stop
           |> List.concat_map ~f:(fun y ->
                  List.range ~stop:`inclusive range.z.start range.z.stop
                  |> List.map ~f:(fun z : Cube.t -> { x; y; z })))
    |> Set.of_list (module Cube)
end

module RebootStep = struct
  type t = power * cuboid

  let parse s : t =
    match String.split s ~on:' ' with
    | [ p; r ] -> (Power.parse p, CuboidRange.parse r |> CuboidRange.to_cuboid)
    | _ -> failwithf "bad parse: %s" s ()
end

let parse_input s = s |> String.split_lines |> List.map ~f:RebootStep.parse
let steps = parse_input input
let reactor : Reactor.t = Set.empty (module Cube)

let () =
  Reactor.run_steps reactor steps
  |> Set.length
  |> printf
       "After executing these steps in the initialization procedure region, %d \
        cubes are on\n\
        %!"
