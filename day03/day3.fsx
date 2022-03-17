open System.IO

type Count =
    { Zeros: int
      Ones: int }
    static member Zero = { Zeros = 0; Ones = 0 }

    static member (+)
        (
            { Zeros = zeros1; Ones = ones1 },
            { Zeros = zeros2; Ones = ones2 }
        ) =
        { Zeros = zeros1 + zeros2
          Ones = ones1 + ones2 }

let incrementCount count bit =
    match bit with
    | '0' -> count + { Count.Zero with Zeros = 1 }
    | '1' -> count + { Count.Zero with Ones = 1 }
    | _ -> failwithf "Bad input bit %c" bit

module Part1 =
    let countBits: seq<Count> -> seq<char> -> seq<Count> =
        Seq.map2 incrementCount

    let counts (input: seq<string>) =
        let numLength = input |> Seq.head |> Seq.length

        input
        |> Seq.fold countBits (Seq.replicate numLength Count.Zero)

    let calcRate comp counts =
        let binary =
            [| for { Zeros = zeros; Ones = ones } in counts do
                   if comp zeros ones then '0' else '1' |]
            |> System.String

        System.Convert.ToUInt32(binary, 2)

    let gammaRate: seq<Count> -> uint32 = calcRate (>)

    let epsilonRate: seq<Count> -> uint32 = calcRate (<)

module Part2 =
    let countBit index acc (num: string) = incrementCount acc num.[index]

    let calcRating rule input =
        let mutable result = input |> Seq.toList
        let mutable i = 0

        while List.length result > 1 do
            let { Zeros = zeros; Ones = ones } =
                result |> List.fold (countBit i) Count.Zero

            result <-
                result
                |> List.filter (fun num -> num.[i] = rule zeros ones)

            i <- i + 1

        System.Convert.ToUInt32(Seq.exactlyOne result, 2)

    let oxygenGeneratorRating: seq<string> -> uint32 =
        calcRating (fun zeros ones -> if zeros > ones then '0' else '1')

    let co2ScrubberRating: seq<string> -> uint32 =
        calcRating (fun zeros ones -> if zeros > ones then '1' else '0')

let fileName =
    fsi.CommandLineArgs
    |> Array.tail
    |> Array.tryHead
    |> Option.defaultValue "input.txt"

let input = File.ReadLines(fileName)
let counts = Part1.counts input

printfn
    "The power consumption of the submarine is %d"
    (Part1.gammaRate counts * Part1.epsilonRate counts)

printfn
    "The life support rating of the submarine is %d"
    (Part2.oxygenGeneratorRating input
     * Part2.co2ScrubberRating input)

module Tests =
    let input =
        "00100
    11110
    10110
    10111
    10101
    01111
    00111
    11100
    10000
    11001
    00010
    01010"
            .Split([| '\n' |])
        |> Array.map (fun s -> s.Trim())

    let counts = Part1.counts input

    let run () =
        printfn
            "%A"
            {| Expected = 22u
               Actual = Part1.gammaRate counts |}

        printfn
            "%A"
            {| Expected = 9u
               Actual = Part1.epsilonRate counts |}

        printfn
            "%A"
            {| Expected = 23u
               Actual = Part2.oxygenGeneratorRating input |}

        printfn
            "%A"
            {| Expected = 10u
               Actual = Part2.co2ScrubberRating input |}
