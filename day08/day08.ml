open Base
open Printf
open Stdio

let failwithf f = ksprintf failwith f
let ( >> ) f g x = g (f x)
let ( << ) f g x = f (g x)

let file_name =
  match Sys.get_argv () |> Array.to_list with
  | _ :: fn :: _ -> fn
  | _ -> "input.txt"

let input = In_channel.read_all file_name
let lines = input |> String.strip |> String.split ~on:'\n' |> Sequence.of_list

let digits =
  lines
  |> Sequence.map ~f:(fun line ->
         match
           line
           |> String.strip
           |> String.split ~on:'|'
           |> List.map ~f:String.strip
         with
         | [ input; output ] ->
             ( String.split ~on:' ' input
               |> List.map ~f:(String.to_list >> Set.of_list (module Char))
             , String.split ~on:' ' output
               |> List.map ~f:(String.to_list >> Set.of_list (module Char)) )
         | _ -> failwithf "bad parse of line %s" line ())

let digits_with_unique_num_of_segments =
  digits
  |> Sequence.fold ~init:Sequence.empty ~f:(fun accum a ->
         a |> snd |> Sequence.of_list |> Sequence.append accum)
  |> Sequence.filter ~f:(fun num ->
         let n = Set.length num in
         n = 2 || n = 3 || n = 4 || n = 7)
  |> Sequence.to_list

let solve nums =
  let five_segment_nums =
    nums |> List.filter ~f:(fun num -> Set.length num = 5)
  in
  let six_segment_nums =
    nums |> List.filter ~f:(fun num -> Set.length num = 6)
  in

  let one =
    nums
    |> List.find ~f:(fun num -> Set.length num = 2)
    |> Option.value_exn ~message:"failed to find 1"
  in
  let seven =
    nums
    |> List.find ~f:(fun num -> Set.length num = 3)
    |> Option.value_exn ~message:"failed to find 7"
  in
  let four =
    nums
    |> List.find ~f:(fun num -> Set.length num = 4)
    |> Option.value_exn ~message:"failed to find 4"
  in
  let eight =
    nums
    |> List.find ~f:(fun num -> Set.length num = 7)
    |> Option.value_exn ~message:"failed to find 8"
  in
  let three =
    five_segment_nums
    |> List.find ~f:(fun num -> Set.diff num one |> Set.length = 3)
    |> Option.value_exn ~message:"failed to find 3"
  in
  let two =
    five_segment_nums
    |> List.find ~f:(fun num ->
           Set.diff (Set.diff num three) four |> Set.length = 1)
    |> Option.value_exn ~message:"failed to find 2"
  in
  let five =
    five_segment_nums
    |> List.find ~f:(fun num -> not (Set.equal num two || Set.equal num three))
    |> Option.value_exn ~message:"failed to find 5"
  in

  let bottom_left_segment =
    Set.nth (Set.diff two three) 0
    |> Option.value_exn ~message:"failed to find bottom left segment"
  in

  let six =
    six_segment_nums
    |> List.find ~f:(fun num ->
           Set.is_subset five ~of_:num
           && Set.exists ~f:(Char.equal bottom_left_segment) num)
    |> Option.value_exn ~message:"failed to find 6"
  in
  let nine =
    six_segment_nums
    |> List.find ~f:(fun num ->
           Set.is_subset five ~of_:num
           && not (Set.exists ~f:(Char.equal bottom_left_segment) num))
    |> Option.value_exn ~message:"failed to find 9"
  in
  let zero =
    six_segment_nums
    |> List.find ~f:(fun num -> not (Set.equal num six || Set.equal num nine))
    |> Option.value_exn ~message:"failed to find 0"
  in
  [ (zero, '0')
  ; (one, '1')
  ; (two, '2')
  ; (three, '3')
  ; (four, '4')
  ; (five, '5')
  ; (six, '6')
  ; (seven, '7')
  ; (eight, '8')
  ; (nine, '9')
  ]

let lookup key list =
  List.find list ~f:(fun (k, _) -> Set.equal key k)
  |> Option.map ~f:snd
  |> Option.value_exn
       ~message:
         ("key not found: " ^ (key |> Set.to_list |> String.of_char_list))

let to_number digits output =
  output
  |> List.map ~f:(fun num -> lookup num digits)
  |> String.of_char_list
  |> Int.of_string

let () =
  digits_with_unique_num_of_segments
  |> List.length
  |> printf "In the output values, the digits 1, 4, 7, and 8 appear %d times\n";

  digits
  |> Sequence.map ~f:(fun (input, output) ->
         let digits = solve input in
         to_number digits output)
  |> Sequence.fold ~init:0 ~f:( + )
  |> printf "Adding up all of the output values produces %d\n"
