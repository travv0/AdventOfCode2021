open Base
open Stdio
open Printf

type count = { zeros : int; ones : int }

module Count = struct
  let zero = { zeros = 0; ones = 0 }
end

let increment_count count bit =
  match bit with
  | '0' -> { count with zeros = count.zeros + 1 }
  | '1' -> { count with ones = count.ones + 1 }
  | _ -> ksprintf failwith "Bad input bit %c" bit

let count_bits = List.map2_exn ~f:increment_count

let counts input =
  let num_length = input |> List.hd_exn |> String.length in
  input
  |> List.map ~f:String.to_list
  |> List.fold ~f:count_bits
       ~init:(List.init num_length ~f:(Fn.const Count.zero))

let calc_rate comp counts =
  let binary =
    counts
    |> List.map ~f:(fun { zeros; ones } -> if comp zeros ones then '0' else '1')
    |> String.of_char_list
    |> ( ^ ) "0b"
  in
  Int.of_string binary

let gamma_rate = calc_rate ( > )
let epsilon_rate = calc_rate ( < )
let count_bit index acc num = increment_count acc num.[index]

let calc_rating (rule : int -> int -> char) input =
  let result = ref input in
  let i = ref 0 in

  while List.length !result > 1 do
    let { zeros; ones } =
      !result |> List.fold ~f:(count_bit !i) ~init:Count.zero
    in
    result :=
      !result |> List.filter ~f:(fun num -> Char.(num.[!i] = rule zeros ones));
    i := !i + 1
  done;

  List.hd_exn !result |> ( ^ ) "0b" |> Int.of_string

let oxygen_generator_rating =
  calc_rating (fun zeros ones -> if zeros > ones then '0' else '1')

let co2_scrubber_rating =
  calc_rating (fun zeros ones -> if zeros > ones then '1' else '0')

let file_name =
  match Sys.get_argv () |> Array.to_list with
  | _ :: fn :: _ -> fn
  | _ -> "input.txt"

let input = In_channel.read_lines file_name
let counts = counts input

let () =
  printf "The power consumption of the submarine is %d\n"
    (gamma_rate counts * epsilon_rate counts);
  printf "The life support rating of the submarine is %d\n"
    (oxygen_generator_rating input * co2_scrubber_rating input)
