open System
open System.IO

let fileName =
    match fsi.CommandLineArgs |> Array.toList with
    | _ :: fn :: _ -> fn
    | _ -> "input.txt"

let input = File.ReadAllText(fileName)

type Polymer = Map<string, int64>
type Rules = Map<string, char>
type ParseResult = { Polymer: Polymer; Rules: Rules }

module Map =
    let ofListFold
        (folder: 'T -> 'T -> 'T)
        (state: 'T)
        (elements: list<'Key * 'T>)
        : Map<'Key, 'T> =
        List.fold
            (fun map (k, v) ->
                if Map.containsKey k map then
                    Map.add k (folder (Map.find k map) v) map
                else
                    Map.add k (folder state v) map)
            Map.empty
            elements

let tails l =
    let rec tails' l acc =
        match l with
        | [] -> acc |> List.rev
        | _ :: rest -> tails' rest (l :: acc)

    tails' l []

let makePolymer s =
    s
    |> List.ofSeq
    |> tails
    |> List.choose
        (function
        | a :: b :: _ -> Some(String([| a; b |]), 1L)
        | a :: _ -> Some(String([| a; ' ' |]), 1)
        | _ -> None)
    |> Map.ofListFold (+) 0

let parseInput (input: string) : ParseResult =
    match input.Split([| "\r\n\r\n"; "\n\n" |], StringSplitOptions.TrimEntries) with
    | [| polymer; rules |] ->
        let polymer = makePolymer polymer

        let rules =
            rules.Split('\n')
            |> Array.map
                (fun line ->
                    match line.Split(" -> ") with
                    | [| from; to_ |] -> (from, to_.[0])
                    | _ -> failwithf "bad parse: %s" line)
            |> Map.ofSeq

        { Polymer = polymer; Rules = rules }
    | _ -> failwithf "bad parse: %s" input

let updatePolymer (rules: Rules) (k: string, v) =
    match Map.tryFind k rules with
    | Some insertion ->
        [ [| k.[0]; insertion |]
          [| insertion; k.[1] |] ]
        |> List.map (fun l -> (String(l), v))
    | None -> [ (k, v) ]

let step (rules: Rules) polymer : Polymer =
    polymer
    |> Map.toList
    |> List.map (updatePolymer rules)
    |> List.concat
    |> Map.ofListFold (+) 0

let rec stepTimes n (rules: Rules) polymer =
    if n < 1 then
        polymer
    else
        stepTimes (n - 1) rules (step rules polymer)

let { Rules = rules; Polymer = polymer } = parseInput input

let subtractLeastCommonFromMostCommon polymer =
    let sorted =
        polymer
        |> Map.toList
        |> List.map (fun (a: string, b) -> (a.[0], b))
        |> Map.ofListFold (+) 0L
        |> Map.toList
        |> List.sortBy snd

    let least = List.head sorted
    let most = List.last sorted
    snd most - snd least

stepTimes 10 rules polymer
|> subtractLeastCommonFromMostCommon
|> printfn
    "After 10 steps, the quantity of the least common element subtracted from the quantity of the most common element is %d"

stepTimes 40 rules polymer
|> subtractLeastCommonFromMostCommon
|> printfn
    "After 40 steps, the quantity of the least common element subtracted from the quantity of the most common element is %d"
