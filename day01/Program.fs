open System.IO

let lines = File.ReadAllLines("input.txt")

module Part1 =
    let mutable prev = lines |> Seq.head |> int

    let increases =
        seq {
            for line in Seq.tail lines do
                let curr = int line
                let result = curr > prev
                prev <- curr
                result
        }

    increases
    |> Seq.where id
    |> Seq.length
    |> printfn "Number of times depth increases: %d"

module Part2 =
    let tails l =
        let rec tails' l acc =
            match l with
            | [] -> acc |> List.rev
            | _ :: rest -> tails' rest (l :: acc)

        tails' (List.ofSeq l) []

    let inline sumGroup l =
        match l with
        | a :: b :: c :: _ -> Some(int a + int b + int c)
        | _ -> None

    let sums = lines |> tails |> Seq.choose sumGroup

    let mutable prev = sums |> Seq.head

    let increases =
        seq {
            for sum in Seq.tail sums do
                let result = sum > prev
                prev <- sum
                result
        }

    increases
    |> Seq.where id
    |> Seq.length
    |> printfn "Number of times sum in sliding window increases: %d"
