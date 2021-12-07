open System.IO

let lines =
    File.ReadAllLines("input.txt") |> Seq.map int

let countIncreases xs =
    let mutable prev = Seq.head xs

    seq {
        for x in Seq.tail xs do
            if x > prev then yield ()
            prev <- x
    }
    |> Seq.length

module Part1 =
    countIncreases lines
    |> printfn "Number of times depth increases: %d"

module Part2 =
    let tails l =
        let rec tails' l acc =
            match l with
            | [] -> acc |> List.rev
            | _ :: rest -> tails' rest (l :: acc)

        tails' (List.ofSeq l) []

    let sumGroup l =
        match l with
        | a :: b :: c :: _ -> Some(a + b + c)
        | _ -> None

    let sums = lines |> tails |> Seq.choose sumGroup

    countIncreases sums
    |> printfn "Number of times sum in sliding window increases: %d"
