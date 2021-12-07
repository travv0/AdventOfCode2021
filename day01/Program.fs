open System.IO

let lines =
    File.ReadAllLines("input.txt") |> Seq.map int

module Part1 =
    let increases =
        let mutable prev = lines |> Seq.head

        seq {
            for curr in Seq.tail lines do
                if curr > prev then yield ()
                prev <- curr
        }
        |> Seq.length

    printfn "Number of times depth increases: %d" increases

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

    let increases =
        let mutable prev = sums |> Seq.head

        seq {
            for sum in Seq.tail sums do
                if sum > prev then yield ()
                prev <- sum
        }
        |> Seq.length

    printfn "Number of times sum in sliding window increases: %d" increases
