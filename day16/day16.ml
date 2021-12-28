open Base
open Stdio
open Printf

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
  |> String.to_list |> List.map ~f:char_to_bit

type packet_data = Literal of int | Operator of packet list
and packet = { version : int; data : packet_data }

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

let rec parse_operator (binary : binary) : packet list * binary =
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
  | _ -> failwith "parse_operator: empty binary"

and parse_packet (binary : binary) : packet * binary =
  let v_str, binary = List.split_n binary 3 in
  let version = binary_to_int v_str in
  let t_str, binary = List.split_n binary 3 in
  let data, remaining =
    match binary_to_int t_str with
    | 4 ->
        let literal, remaining = parse_literal binary in
        (Literal literal, remaining)
    | _ ->
        let operator, remaining = parse_operator binary in
        (Operator operator, remaining)
  in
  ({ version; data }, remaining)

let binary = input |> String.strip |> hex_to_binary
let packet = parse_packet binary |> fst

let rec sum_versions packet =
  packet.version
  +
  match packet.data with
  | Operator op -> List.map ~f:sum_versions op |> List.fold ~init:0 ~f:( + )
  | _ -> 0

let () =
  sum_versions packet
  |> printf "The version sum of the package hierarchy is %d\n"
