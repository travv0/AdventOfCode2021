open System.IO

let fileName =
    match fsi.CommandLineArgs |> Array.toList with
    | _ :: fn :: _ -> fn
    | _ -> "input.txt"

let lines =
    File.ReadAllLines(fileName) |> Seq.map int

let countIncreases xs =
    let mutable prev = Seq.head xs

    seq {
        for x in Seq.tail xs do
            if x > prev then yield ()
            prev <- x
    }
    |> Seq.length

countIncreases lines
|> printfn "Number of times depth increases: %d"

let tails l =
    let rec tails' l acc =
        match l with
        | [] -> acc |> List.rev
        | _ :: rest -> tails' rest (l :: acc)

    tails' (List.ofSeq l) []

let sumGroup =
    function
    | a :: b :: c :: _ -> Some(a + b + c)
    | _ -> None

let sums = lines |> tails |> Seq.choose sumGroup

countIncreases sums
|> printfn "Number of times sum in sliding window increases: %d"
