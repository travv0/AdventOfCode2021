open System
open System.IO

type Digit = Set<char>

let fileName =
    match fsi.CommandLineArgs |> Array.toList with
    | _ :: fn :: _ -> fn
    | _ -> "input.txt"

let input = File.ReadAllText fileName

let lines =
    input.Split(
        '\n',
        StringSplitOptions.RemoveEmptyEntries
        ||| StringSplitOptions.TrimEntries
    )

let digits =
    lines
    |> Array.map (fun line ->
        match
            line.Split
                (
                    '|',
                    StringSplitOptions.RemoveEmptyEntries
                    ||| StringSplitOptions.TrimEntries
                )
            with
        | [| input; output |] -> input.Split(' ') |> Array.map Set.ofSeq, output.Split(' ') |> Array.map Set.ofSeq
        | _ -> failwithf "bad parse of line %s" line)

let digitsWithUniqueNumOfSegments =
    digits
    |> Seq.fold (fun accum a -> a |> snd |> Seq.ofArray |> Seq.append accum) Seq.empty
    |> Seq.filter (fun num -> let n = Set.count num in n = 2 || n = 3 || n = 4 || n = 7)
    |> Seq.toList

module Option =
    let defaultFailWith message =
        Option.defaultWith (fun () -> failwith message)

let solve (nums: seq<Digit>) =
    let fiveSegmentNums =
        nums |> Seq.filter (fun num -> Set.count num = 5)

    let sixSegmentNums =
        nums |> Seq.filter (fun num -> Set.count num = 6)

    let one =
        nums
        |> Seq.tryFind (fun num -> Set.count num = 2)
        |> Option.defaultFailWith "failed to find 1"

    let seven =
        nums
        |> Seq.tryFind (fun num -> Set.count num = 3)
        |> Option.defaultFailWith "failed to find 7"

    let four =
        nums
        |> Seq.tryFind (fun num -> Set.count num = 4)
        |> Option.defaultFailWith "failed to find 4"

    let eight =
        nums
        |> Seq.tryFind (fun num -> Set.count num = 7)
        |> Option.defaultFailWith "failed to find 8"

    let three =
        fiveSegmentNums
        |> Seq.tryFind (fun num -> Set.difference num one |> Set.count = 3)
        |> Option.defaultFailWith "failed to find 3"

    let two =
        fiveSegmentNums
        |> Seq.tryFind (fun num ->
            Set.difference (Set.difference num three) four
            |> Set.count = 1)
        |> Option.defaultFailWith "failed to find 2"

    let five =
        fiveSegmentNums
        |> Seq.tryFind (fun num -> not (num = two || num = three))
        |> Option.defaultFailWith "failed to find 5"

    let bottomLeftSegment =
        Set.difference two three
        |> Seq.tryItem 0
        |> Option.defaultFailWith "failed to find bottom left segment"

    let six =
        sixSegmentNums
        |> Seq.tryFind (fun num ->
            Set.isSubset five num
            && Set.exists ((=) bottomLeftSegment) num)
        |> Option.defaultFailWith "failed to find 6"

    let nine =
        sixSegmentNums
        |> Seq.tryFind (fun num ->
            Set.isSubset five num
            && not (Set.exists ((=) bottomLeftSegment) num))
        |> Option.defaultFailWith "failed to find 9"

    let zero =
        sixSegmentNums
        |> Seq.tryFind (fun num -> not (num = six || num = nine))
        |> Option.defaultFailWith "failed to find 0"

    Map.ofList [ (zero, '0')
                 (one, '1')
                 (two, '2')
                 (three, '3')
                 (four, '4')
                 (five, '5')
                 (six, '6')
                 (seven, '7')
                 (eight, '8')
                 (nine, '9') ]

let toNumber digits output =
    output
    |> Seq.map (fun num -> Map.find num digits)
    |> Array.ofSeq
    |> String
    |> int

digitsWithUniqueNumOfSegments
|> List.length
|> printfn "In the output values, the digits 1, 4, 7, and 8 appear %d times"

digits
|> Seq.map (fun (input, output) ->
    let digits = solve input
    toNumber digits output)
|> Seq.fold (+) 0
|> printfn "Adding up all of the output values produces %d"
