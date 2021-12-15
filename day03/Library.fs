module Day3

open FSharpPlus

type Count =
    { Zeros: int
      Ones: int }
    static member Zero = { Zeros = 0; Ones = 0 }

    static member (+)({ Zeros = zeros1; Ones = ones1 }, { Zeros = zeros2; Ones = ones2 }) =
        { Zeros = zeros1 + zeros2
          Ones = ones1 + ones2 }

let incrementCount count bit =
    match bit with
    | '0' -> count ++ { zero with Zeros = 1 }
    | '1' -> count ++ { zero with Ones = 1 }
    | _ -> failwithf "Bad input bit %c" bit

module Part1 =
    let countBits: seq<Count> -> seq<char> -> seq<Count> = Seq.map2 incrementCount

    let counts (input: seq<string>) =
        let numLength = input |> head |> length

        input
        |> fold countBits (Seq.replicate numLength zero)

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
        let mutable result = input |> toList
        let mutable i = 0

        while length result > 1 do
            let { Zeros = zeros; Ones = ones } = result |> fold (countBit i) zero

            result <-
                result
                |> filter (fun num -> num.[i] = rule zeros ones)

            i <- i + 1

        System.Convert.ToUInt32(Seq.exactlyOne result, 2)

    let oxygenGeneratorRating: seq<string> -> uint32 =
        calcRating (fun zeros ones -> if zeros > ones then '0' else '1')

    let co2ScrubberRating: seq<string> -> uint32 =
        calcRating (fun zeros ones -> if zeros > ones then '1' else '0')
