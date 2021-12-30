open Base
open Stdio
open Printf

let file_name =
  match Sys.get_argv () |> Array.to_list with
  | _ :: fn :: _ -> fn
  | _ -> "input.txt"

let input = In_channel.read_all file_name

module SnailfishNumber = struct
  type t = Number of int | Pair of (t * t)

  let rec to_string = function
    | Pair (a, b) -> sprintf "[%s,%s]" (to_string a) (to_string b)
    | Number n -> Int.to_string n

  let rec add_to_left num ~count =
    match num with
    | Pair (a, b) -> Pair (add_to_left a ~count, b)
    | Number n -> Number (n + count)

  let rec add_to_right num ~count =
    match num with
    | Pair (a, b) -> Pair (a, add_to_right b ~count)
    | Number n -> Number (n + count)

  let rec edge_explode_left num : t =
    match num with
    | Pair (Pair (Number _, Number b), c) ->
        Pair (Number 0, add_to_left c ~count:b)
    | Pair (a, b) -> Pair (edge_explode_left a, b)
    | Number _ -> failwith "edge_explode_left: hit dead end"

  let rec edge_explode_right num : t =
    match num with
    | Pair (a, Pair (Number b, Number _)) ->
        Pair (add_to_right a ~count:b, Number 0)
    | Pair (a, b) -> Pair (a, edge_explode_right b)
    | Number _ -> failwith "edge_explode_right: hit dead end"

  let rec walk_path num ~path =
    match (path, num) with
    | [], _ -> num
    | `L :: path, Pair (a, _) -> walk_path a ~path
    | `R :: path, Pair (_, b) -> walk_path b ~path
    | _, Number _ -> failwith "walk_path: hit number"

  let explode_right num ~path : t =
    match num with
    | Pair (a, b) -> (
        match walk_path num ~path with
        | Pair (Number n, Number _) ->
            Pair (add_to_right a ~count:n, edge_explode_left b)
        | _ -> failwith "explode_right: path didn't lead to pair of numbers")
    | Number _ -> failwith "explode_right: hit number"

  let explode_left num ~path : t =
    match num with
    | Pair (a, b) -> (
        match walk_path num ~path with
        | Pair (Number _, Number m) ->
            Pair (edge_explode_right a, add_to_left b ~count:m)
        | _ -> failwith "explode_left: path didn't lead to pair of numbers")
    | Number _ -> failwith "explode_left: hit number"

  let rec explode num ~path =
    if
      List.for_all path ~f:(fun dir ->
          match dir with `L -> true | `R -> false)
    then edge_explode_left num
    else if
      List.for_all path ~f:(fun dir ->
          match dir with `R -> true | `L -> false)
    then edge_explode_right num
    else
      match path with
      | dir :: tl -> (
          if
            tl
            |> List.for_all ~f:(fun dir ->
                   match dir with `L -> true | `R -> false)
          then explode_right num ~path
          else if
            tl
            |> List.for_all ~f:(fun dir ->
                   match dir with `R -> true | `L -> false)
          then explode_left num ~path
          else
            match (dir, num) with
            | `L, Pair (a, b) -> Pair (explode a ~path:tl, b)
            | `R, Pair (a, b) -> Pair (a, explode b ~path:tl)
            | _, _ ->
                failwithf "explode: bad path for num %s" (to_string num) ())
      | _ -> failwith "explode: empty path"

  let rec explode_path num ~path : _ list =
    match num with
    | Pair (a, b) ->
        if List.length path >= 4 then List.rev path
        else
          let left_path = explode_path a ~path:(`L :: path) in
          if List.length left_path >= 4 then left_path
          else
            let right_path = explode_path b ~path:(`R :: path) in
            if List.length right_path >= 4 then right_path else []
    | Number _ -> []

  let split n =
    Pair (Number (n / 2), Number Float.(of_int n / 2. |> round_up |> to_int))

  let rec maybe_split : t -> t * bool = function
    | Pair (a, b) as pair ->
        let num, split_done = maybe_split a in
        if split_done then (Pair (num, b), true)
        else
          let num, split_done = maybe_split b in
          if split_done then (Pair (a, num), true) else (pair, false)
    | Number n as num -> if n >= 10 then (split n, true) else (num, false)

  let rec reduce (num : t) =
    let path = explode_path num ~path:[] in
    if List.length path > 0 then explode num ~path |> reduce
    else
      let new_num, split_done = maybe_split num in
      if split_done then reduce new_num else num

  let rec magnitude = function
    | Number n -> n
    | Pair (a, b) -> (3 * magnitude a) + (2 * magnitude b)

  let ( + ) num1 num2 = Pair (num1, num2) |> reduce

  let of_string s =
    let parse_num cs : t * char list =
      let digits, remaining = List.split_while ~f:Char.is_digit cs in
      (Number (String.of_char_list digits |> Int.of_string), remaining)
    in
    let rec parse_pair (cs : char list) : t * char list =
      let first_elem, cs = parse_element cs in
      let cs = List.drop_while cs ~f:Char.(( = ) ',') in
      let second_elem, cs = parse_element cs in
      let cs =
        match cs with
        | ']' :: cs -> cs
        | c :: _ -> failwithf "parse_pair: no closing bracket, found '%c'" c ()
        | [] -> failwith "parse_pair: hit end of string"
      in
      (Pair (first_elem, second_elem), cs)
    and parse_element (cs : char list) : t * char list =
      match cs with '[' :: cs -> parse_pair cs | _ -> parse_num cs
    in

    s |> String.to_list |> parse_element |> fst
end

let () =
  let open SnailfishNumber in
  let nums =
    input |> String.strip |> String.split_lines |> List.map ~f:of_string
  in

  nums
  |> List.reduce_exn ~f:( + )
  |> magnitude
  |> printf "The magnitude of the final sum is %d\n";

  List.concat_mapi nums ~f:(fun i n ->
      List.filter_mapi nums ~f:(fun j m ->
          if i <> j then Some (n + m |> magnitude) else None))
  |> List.max_elt ~compare:Int.compare
  |> Option.value_exn ~message:"max_elt called on empty list"
  |> printf
       "The largest magnitude of any sum of two different snailfish numbers \
        from the homework assignment is %d\n"
