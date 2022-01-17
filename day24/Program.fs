#!/usr/bin/env -S dotnet fsi

open System.IO

type Var =
    | W
    | X
    | Y
    | Z

module Var =
    let parse =
        function
        | "w" -> W
        | "x" -> X
        | "y" -> Y
        | "z" -> Z
        | s -> failwithf "invalid variable %s" s

type Vars = Map<Var, int64>

module Vars =
    let get v vars =
        Map.tryFind v vars |> Option.defaultValue 0L

type Val =
    | Var of Var
    | Val of int64

    member v.Value vars =
        match v with
        | Val v -> v
        | Var v -> Vars.get v vars

module Val =
    let parse (s: string) =
        try
            Val(int64 s)
        with
        | _ ->
            Var
            <| match s with
               | "w" -> W
               | "x" -> X
               | "y" -> Y
               | "z" -> Z
               | s -> failwithf "invalid variable %s for val" s

type Instruction =
    | Inp of Var
    | Add of Var * Val
    | Mul of Var * Val
    | Div of Var * Val
    | Mod of Var * Val
    | Eql of Var * Val

module Instruction =
    let parse (s: string) =
        match s.Trim().Split(' ') with
        | [| "inp"; a |] -> Inp(Var.parse a)
        | [| "add"; a; b |] -> Add(Var.parse a, Val.parse b)
        | [| "mul"; a; b |] -> Mul(Var.parse a, Val.parse b)
        | [| "div"; a; b |] -> Div(Var.parse a, Val.parse b)
        | [| "mod"; a; b |] -> Mod(Var.parse a, Val.parse b)
        | [| "eql"; a; b |] -> Eql(Var.parse a, Val.parse b)
        | _ -> failwithf "bad parse: %s" s

    let run vars instruction inputs =
        match instruction, inputs with
        | Inp a, i :: rest -> Map.add a i vars, rest
        | Inp a, [] -> failwith "out of input"
        | Add (a, b), rest -> Map.add a (Vars.get a vars + b.Value vars) vars, rest
        | Mul (a, b), rest -> Map.add a (Vars.get a vars * b.Value vars) vars, rest
        | Div (a, b), rest -> Map.add a (Vars.get a vars / b.Value vars) vars, rest
        | Mod (a, b), rest ->
            let av = Vars.get a vars
            let bv = b.Value vars

            if av < 0 || bv <= 0 then
                failwithf "bad input to mod: a=%d b=%d" av bv
            else
                Map.add a (av % bv) vars, rest
        | Eql (a, b), rest ->
            Map.add
                a
                (if Vars.get a vars = b.Value vars then
                     1
                 else
                     0)
                vars,
            rest

let parse (s: string) =
    s.Trim().Split('\n') |> Seq.map Instruction.parse

let run instructions inputs =
    Seq.fold
        (fun (vars, inputs) instruction -> Instruction.run vars instruction inputs)
        (Map.empty, inputs)
        instructions
    |> fst

let instructions =
    File.ReadAllText("input.txt")
    |> parse
    |> List.ofSeq

let isValidSerialNumber (number: list<int64>) =
    let vars = run instructions number
    Vars.get Z vars = 0

let isUnrestricted (number: int64 []) =
    ((((number.[0] + 6L)
       + (number.[1] + 12L)
       + (number.[2] + 8L))
      / 26L)
     + (((number.[3] + 7L)
         + (number.[4] + 7L)
         + (number.[5] + 2L)
         + (number.[6] + 12L))
        / 26L)
     + (((number.[7] + 15L) + (number.[8] + 4L)) / 26L)
     + ((number.[9] + 5L) / 26L)
     + ((number.[10] + 12L) / 26L)
     + ((number.[11] + 11L) / 26L)
     + ((number.[12] + 13L) / 26L)
     + ((number.[13] + 7L))) % 26L = 0L

seq { 99999999999999L .. -1L .. 11111111111111L }
|> Seq.map (sprintf "%d")
|> Seq.filter (String.forall ((<>) '0'))
|> Seq.map (Array.ofSeq >> Array.map (sprintf "%c" >> int64))
|> Seq.filter isUnrestricted
|> Seq.chunkBySize 20
|> Seq.map (fun chunk ->
    Array.Parallel.map
        (fun number ->
            // if number.[10] = '9' then
            //     printfn "%s" number

            number, number |> Seq.toList |> isValidSerialNumber)
        chunk)
|> Seq.concat
|> Seq.filter snd
|> Seq.head
|> fst
|> Array.map (sprintf "%d")
|> String.concat ""
|> printfn "The largest model number accepted by MONAD is %s"
