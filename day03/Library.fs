module Day3

type Count = { Zeros: int; Ones: int }

module Count =
    let empty = { Zeros = 0; Ones = 0 }

let incrementCount count bit =
    match bit with
    | '0' -> { count with Zeros = count.Zeros + 1 }
    | '1' -> { count with Ones = count.Ones + 1 }
    | _ -> failwithf "Bad input bit %c" bit

module Part1 =
    let countBits: Count seq -> char seq -> Count seq = Seq.map2 incrementCount

    let counts input =
        let numLength = input |> Seq.head |> Seq.length

        input
        |> Seq.fold countBits (Seq.replicate numLength Count.empty)

    let calcRate comp counts =
        let binary =
            [| for { Zeros = zeros; Ones = ones } in counts do
                   if comp zeros ones then '0' else '1' |]
            |> System.String

        System.Convert.ToUInt32(binary, 2)

    let gammaRate: Count seq -> uint32 = calcRate (>)

    let epsilonRate: Count seq -> uint32 = calcRate (<)

module Part2 =
    let countBit index acc (num: string) = incrementCount acc num.[index]

    let calcRating rule input =
        let mutable result = input |> List.ofSeq
        let mutable i = 0

        while Seq.length result > 1 do
            let { Zeros = zeros; Ones = ones } =
                result |> Seq.fold (countBit i) Count.empty

            result <-
                result
                |> List.filter (fun num -> num.[i] = rule zeros ones)

            i <- i + 1

        System.Convert.ToUInt32(Seq.exactlyOne result, 2)

    let oxygenGeneratorRating: string seq -> uint32 =
        calcRating (fun zeros ones -> if zeros > ones then '0' else '1')

    let co2ScrubberRating: string seq -> uint32 =
        calcRating (fun zeros ones -> if zeros > ones then '1' else '0')
