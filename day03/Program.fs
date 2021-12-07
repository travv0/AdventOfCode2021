open System.IO

let input = File.ReadLines("input.txt")

let numLength = input |> Seq.head |> Seq.length

type Count = { Zeros: int; Ones: int }

let incrementCount bit count =
    if bit = '0' then
        { count with Zeros = count.Zeros + 1 }
    elif bit = '1' then
        { count with Ones = count.Ones + 1 }
    else
        failwithf "Bad input bit %c" bit

module Part1 =
    let countBits acc num =
        seq {
            for count, bit in Seq.zip acc num do
                incrementCount bit count
        }

    let counts =
        input
        |> Seq.fold countBits (Seq.replicate numLength { Zeros = 0; Ones = 0 })

    let gammaRateBinary =
        [| for { Zeros = zeros; Ones = ones } in counts do
               if zeros > ones then '0' else '1' |]
        |> System.String

    let gammaRate =
        System.Convert.ToUInt32(gammaRateBinary, 2)

    let epsilonRateBinary =
        [| for { Zeros = zeros; Ones = ones } in counts do
               if zeros < ones then '0' else '1' |]
        |> System.String

    let epsilonRate =
        System.Convert.ToUInt32(epsilonRateBinary, 2)

    printfn "The power consumption of the submarine is %d" (gammaRate * epsilonRate)

module Part2 =
    let countBit index acc (num: string) = incrementCount num.[index] acc

    let calcRating rule =
        let mutable result = input

        for i in 0 .. numLength - 1 do
            if Seq.length result > 1 then
                let { Zeros = zeros; Ones = ones } =
                    result
                    |> Seq.fold (countBit i) { Zeros = 0; Ones = 0 }

                result <-
                    result
                    |> Seq.filter (fun num -> num.[i] = rule zeros ones)

        Seq.exactlyOne result

    let oxygenGeneratorRatingBinary =
        calcRating (fun zeros ones -> if zeros > ones then '0' else '1')

    let co2ScrubberRatingBinary =
        calcRating (fun zeros ones -> if zeros > ones then '1' else '0')

    let oxygenGeneratorRating =
        System.Convert.ToUInt32(oxygenGeneratorRatingBinary, 2)

    let co2ScrubberRating =
        System.Convert.ToUInt32(co2ScrubberRatingBinary, 2)

    printfn "The life support rating of the submarine is %d" (oxygenGeneratorRating * co2ScrubberRating)
