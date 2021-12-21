open Base
open Printf
open Stdio

let failwithf f = ksprintf failwith f

type token =
  | Open_paren
  | Open_bracket
  | Open_brace
  | Open_pointy
  | Close_paren
  | Close_bracket
  | Close_brace
  | Close_pointy

type line_type = Corrupted of token | Incomplete of token list

let file_name =
  match Sys.get_argv () |> Array.to_list with
  | _ :: fn :: _ -> fn
  | _ -> "input.txt"

let tokenize = function
  | '(' -> Open_paren
  | '[' -> Open_bracket
  | '{' -> Open_brace
  | '<' -> Open_pointy
  | ')' -> Close_paren
  | ']' -> Close_bracket
  | '}' -> Close_brace
  | '>' -> Close_pointy
  | c -> failwithf "bad parse: %c" c

let untokenize = function
  | Open_paren -> '('
  | Open_bracket -> '['
  | Open_brace -> '{'
  | Open_pointy -> '<'
  | Close_paren -> ')'
  | Close_bracket -> ']'
  | Close_brace -> '}'
  | Close_pointy -> '>'

let closes token1 token2 =
  match (token1, token2) with
  | Close_paren, Open_paren
  | Close_bracket, Open_bracket
  | Close_brace, Open_brace
  | Close_pointy, Open_pointy ->
      true
  | _ -> false

let flip_token = function
  | Open_paren -> Close_paren
  | Open_bracket -> Close_bracket
  | Open_brace -> Close_brace
  | Open_pointy -> Close_pointy
  | Close_paren -> Open_paren
  | Close_bracket -> Open_bracket
  | Close_brace -> Open_brace
  | Close_pointy -> Open_pointy

let rec autocomplete = function
  | token :: tokens -> flip_token token :: autocomplete tokens
  | tokens -> tokens

let process_line line =
  match String.to_list line with
  | c :: line ->
      let rec go tokens = function
        | c :: line -> (
            let token = tokenize c in
            match token with
            | Open_paren | Open_bracket | Open_brace | Open_pointy ->
                go (token :: tokens) line
            | _ -> (
                match tokens with
                | opening_token :: tokens when closes token opening_token ->
                    go tokens line
                | _ -> Corrupted token))
        | [] -> Incomplete (autocomplete tokens)
      in
      go [ tokenize c ] line
  | _ -> failwith "empty line"

let lines = In_channel.read_lines file_name

let score_syntax_error = function
  | Close_paren -> 3
  | Close_bracket -> 57
  | Close_brace -> 1197
  | Close_pointy -> 25137
  | token -> failwithf "bad closing token: %c" (untokenize token)

let score_autocomplete tokens =
  let s = function
    | Close_paren -> 1
    | Close_bracket -> 2
    | Close_brace -> 3
    | Close_pointy -> 4
    | token -> failwithf "bad closing token: %c" (untokenize token)
  in
  let rec go score = function
    | token :: tokens ->
        let new_score = (score * 5) + s token in
        go new_score tokens
    | _ -> score
  in
  go 0 tokens

let () =
  let processed_lines = lines |> List.map ~f:process_line in
  processed_lines
  |> Sequence.of_list
  |> Sequence.filter_map ~f:(function
       | Corrupted token -> Some token
       | _ -> None)
  |> Sequence.map ~f:score_syntax_error
  |> Sequence.reduce_exn ~f:( + )
  |> printf "The total syntax error score is %d points\n";

  let autocomplete_scores =
    processed_lines
    |> Sequence.of_list
    |> Sequence.filter_map ~f:(function
         | Incomplete tokens -> Some tokens
         | _ -> None)
    |> Sequence.map ~f:score_autocomplete
    |> Sequence.to_list
    |> List.sort ~compare:Int.compare
  in
  List.length autocomplete_scores / 2
  |> List.nth_exn autocomplete_scores
  |> printf "The middle autocomplete score is %d points\n"
