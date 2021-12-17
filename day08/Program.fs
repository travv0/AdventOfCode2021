module Day8

open System
open System.IO

let digitsWithUniqueNumOfSegments (input: string) =
    input.Split(
        '\n',
        StringSplitOptions.RemoveEmptyEntries
        ||| StringSplitOptions.TrimEntries
    )
    |> Array.collect (fun line ->
        line.Split('|', StringSplitOptions.TrimEntries).[1]
            .Split(' '))
    |> Array.filter (fun num -> let n = String.length num in n = 2 || n = 3 || n = 4 || n = 7)

let input = File.ReadAllText("input.txt")

digitsWithUniqueNumOfSegments input
|> Array.length
|> printfn "In the output values, the digits 1, 4, 7, and 8 appear %d times"
