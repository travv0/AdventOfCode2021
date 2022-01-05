open Base
open Stdio
open Printf

module type Monoid = sig
  type t

  val empty : t
  val append : t -> t -> t
end

module MonoidUtils (M : Monoid) = struct
  include M

  let ( ^^ ) x y = append x y
  let concat xs = List.fold_left ~f:( ^^ ) ~init:empty xs
end

type count = { zeros : int; ones : int }

module CountMonoid : Monoid with type t = count = struct
  type t = count

  let empty = { zeros = 0; ones = 0 }

  let append { zeros = z1; ones = o1 } { zeros = z2; ones = o2 } =
    { zeros = z1 + z2; ones = o1 + o2 }
end

module Count = struct
  include MonoidUtils (CountMonoid)
end

let increment_count count bit =
  let open Count in
  match bit with
  | '0' -> count ^^ { empty with zeros = 1 }
  | '1' -> count ^^ { empty with ones = 1 }
  | _ -> ksprintf failwith "Bad input bit %c" bit

let count_bits = List.map2_exn ~f:increment_count

let counts input =
  let num_length = input |> List.hd_exn |> String.length in
  input
  |> List.map ~f:String.to_list
  |> List.fold ~f:count_bits
       ~init:(List.init num_length ~f:(Fn.const Count.empty))

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
      !result |> List.fold ~f:(count_bit !i) ~init:Count.empty
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
