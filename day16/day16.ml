open Base
open Stdio
open Printf

let ( >> ) f g x = g (f x)

let file_name =
  match Sys.get_argv () |> Array.to_list with
  | _ :: fn :: _ -> fn
  | _ -> "input.txt"

let input = In_channel.read_all file_name

type bit = Zero | One
type binary = bit list

let hex_digit_to_binary = function
  | '0' -> "0000"
  | '1' -> "0001"
  | '2' -> "0010"
  | '3' -> "0011"
  | '4' -> "0100"
  | '5' -> "0101"
  | '6' -> "0110"
  | '7' -> "0111"
  | '8' -> "1000"
  | '9' -> "1001"
  | 'A' | 'a' -> "1010"
  | 'B' | 'b' -> "1011"
  | 'C' | 'c' -> "1100"
  | 'D' | 'd' -> "1101"
  | 'E' | 'e' -> "1110"
  | 'F' | 'f' -> "1111"
  | c -> failwithf "bad hex: %c" c ()

let char_to_bit = function
  | '0' -> Zero
  | '1' -> One
  | c -> failwithf "bad bit: %c" c ()

let bit_to_char = function Zero -> '0' | One -> '1'

let binary_to_string binary =
  List.map binary ~f:bit_to_char |> String.of_char_list

let hex_to_binary hex : binary =
  String.concat_map hex ~f:hex_digit_to_binary
  |> String.to_list
  |> List.map ~f:char_to_bit

type packet_data =
  | Sum of packet list
  | Product of packet list
  | Minimum of packet list
  | Maximum of packet list
  | Literal of int
  | GreaterThan of (packet * packet)
  | LessThan of (packet * packet)
  | EqualTo of (packet * packet)

and packet = { version : int; data : packet_data }

let rec value packet : int =
  match packet.data with
  | Sum sub_packets ->
      sub_packets |> List.map ~f:value |> List.fold ~init:0 ~f:( + )
  | Product sub_packets ->
      sub_packets |> List.map ~f:value |> List.fold ~init:1 ~f:( * )
  | Minimum sub_packets ->
      sub_packets
      |> List.map ~f:value
      |> List.min_elt ~compare:Int.compare
      |> Option.value_exn ~message:"no minimum value found"
  | Maximum sub_packets ->
      sub_packets
      |> List.map ~f:value
      |> List.max_elt ~compare:Int.compare
      |> Option.value_exn ~message:"no maximum value found"
  | Literal i -> i
  | GreaterThan (a, b) -> if value a > value b then 1 else 0
  | LessThan (a, b) -> if value a < value b then 1 else 0
  | EqualTo (a, b) -> if value a = value b then 1 else 0

let binary_to_int (binary : binary) =
  Int.of_string ("0b" ^ binary_to_string binary)

let parse_literal (binary : binary) : int * binary =
  let rec parse_literal' num = function
    | check_bit :: binary -> (
        let chunk, remaining = List.split_n binary 4 in
        match check_bit with
        | Zero -> (binary_to_int (num @ chunk), remaining)
        | One -> parse_literal' (num @ chunk) remaining)
    | _ -> failwith "parse_literal': empty binary"
  in
  parse_literal' [] binary

let rec parse_many (binary : binary) : packet list * binary =
  match binary with
  | Zero :: binary ->
      let length_bin, binary = List.split_n binary 15 in
      let length = binary_to_int length_bin in
      let binary, remaining = List.split_n binary length in

      let rec parse_packets packets binary =
        match parse_packet binary with
        | packet, [] -> List.rev (packet :: packets)
        | packet, binary -> parse_packets (packet :: packets) binary
      in

      (parse_packets [] binary, remaining)
  | One :: binary ->
      let length_bin, binary = List.split_n binary 11 in
      let sub_packet_count = binary_to_int length_bin in

      let rec parse_packets n packets binary =
        let packet, binary = parse_packet binary in
        if n <= 1 then (List.rev (packet :: packets), binary)
        else parse_packets (n - 1) (packet :: packets) binary
      in

      parse_packets sub_packet_count [] binary
  | _ -> failwith "parse_many: empty binary"

and parse_packet (binary : binary) : packet * binary =
  let parse_two =
    parse_many >> function
    | [ a; b ], c -> ((a, b), c)
    | l, _ ->
        failwithf "parse_two: invalid number of elements: %d" (List.length l) ()
  in
  let v, binary = List.split_n binary 3 in
  let version = binary_to_int v in
  let type_, binary = List.split_n binary 3 in
  let data, remaining =
    match binary_to_int type_ with
    | 0 ->
        let packet, remaining = parse_many binary in
        (Sum packet, remaining)
    | 1 ->
        let packet, remaining = parse_many binary in
        (Product packet, remaining)
    | 2 ->
        let packet, remaining = parse_many binary in
        (Minimum packet, remaining)
    | 3 ->
        let packet, remaining = parse_many binary in
        (Maximum packet, remaining)
    | 4 ->
        let literal, remaining = parse_literal binary in
        (Literal literal, remaining)
    | 5 ->
        let packet, remaining = parse_two binary in
        (GreaterThan packet, remaining)
    | 6 ->
        let packet, remaining = parse_two binary in
        (LessThan packet, remaining)
    | 7 ->
        let packet, remaining = parse_two binary in
        (EqualTo packet, remaining)
    | _ -> failwith "parse_packet: bad packet"
  in
  ({ version; data }, remaining)

let binary = input |> String.strip |> hex_to_binary
let packet = parse_packet binary |> fst

let rec sum_versions packet =
  packet.version
  +
  match packet.data with
  | Sum op | Product op | Minimum op | Maximum op ->
      List.map ~f:sum_versions op |> List.fold ~init:0 ~f:( + )
  | GreaterThan (a, b) | LessThan (a, b) | EqualTo (a, b) ->
      sum_versions a + sum_versions b
  | _ -> 0

let () =
  sum_versions packet
  |> printf "The version sum of the package hierarchy is %d\n";

  value packet |> printf "The value of the transmission is %d\n"
