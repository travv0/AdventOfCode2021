open System
open System.IO

let fileName =
    match fsi.CommandLineArgs |> List.ofArray with
    | _ :: fn :: _ -> fn
    | _ -> "input.txt"

let input = File.ReadAllLines fileName

let parseInput (lines: string []) =
    lines
    |> Seq.map
        (fun line ->
            match line.Split('-') with
            | [| from; to_ |] -> [ (from, to_); (to_, from) ]
            | _ -> failwithf "bad line: %s" line)
    |> Seq.concat
    |> Seq.groupBy fst
    |> Seq.map (fun (k, v) -> (k, Seq.map snd v |> Set.ofSeq))
    |> Map.ofSeq

let isSmallCave = String.forall Char.IsLower

type Part =
    | Part1
    | Part2

let walkNodes
    (from: string)
    (to_: string)
    part
    connections
    : list<list<string>> =
    let rec walkNodes'
        from
        (deadNodes: Set<string>)
        (path: list<string>)
        (twoTimeQueue: option<string>)
        : list<list<string>> =
        if from = to_ then
            [ List.rev (to_ :: path) ]
        else
            let validChildren =
                Map.tryFind from connections
                |> Option.map (fun c -> Set.difference c deadNodes)

            match validChildren with
            | None -> []
            | Some children when Set.isEmpty children -> []
            | Some children ->
                let newDeadNodes, newTwoTimeQueue =
                    if isSmallCave from then
                        match twoTimeQueue with
                        | Some cave when cave = from -> (deadNodes, None)
                        | Some cave -> (Set.add from deadNodes, Some cave)
                        | None -> (Set.add from deadNodes, None)
                    else
                        (deadNodes, twoTimeQueue)

                children
                |> Seq.map
                    (fun c ->
                        walkNodes' c newDeadNodes (from :: path) newTwoTimeQueue)
                |> Seq.concat
                |> Seq.toList

    match part with
    | Part1 -> walkNodes' from Set.empty [] None
    | Part2 ->
        let smallCaves =
            Map.keys connections
            |> Seq.filter
                (fun cave -> isSmallCave cave && cave <> from && cave <> to_)

        smallCaves
        |> Seq.map (fun cave -> Some cave |> walkNodes' from Set.empty [])
        |> Seq.concat
        |> Seq.distinct
        |> List.ofSeq

let connections = parseInput input

connections
|> walkNodes "start" "end" Part1
|> List.length
|> printfn
    "There are %d paths through the cave system if each small cave is visited once."

connections
|> walkNodes "start" "end" Part2
|> List.length
|> printfn
    "There are %d paths through the cave system if one small cave can be visited twice."
