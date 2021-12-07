module Day3

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

    let counts input =
        let numLength = input |> Seq.head |> Seq.length

        input
        |> Seq.fold countBits (Seq.replicate numLength { Zeros = 0; Ones = 0 })

    let gammaRateBinary counts =
        [| for { Zeros = zeros; Ones = ones } in counts do
               if zeros > ones then '0' else '1' |]
        |> System.String

    let gammaRate counts =
        System.Convert.ToUInt32(gammaRateBinary counts, 2)

    let epsilonRateBinary counts =
        [| for { Zeros = zeros; Ones = ones } in counts do
               if zeros < ones then '0' else '1' |]
        |> System.String

    let epsilonRate counts =
        System.Convert.ToUInt32(epsilonRateBinary counts, 2)


module Part2 =
    let countBit index acc (num: string) = incrementCount num.[index] acc

    let calcRating input rule =
        let numLength = input |> Seq.head |> Seq.length
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

    let oxygenGeneratorRatingBinary input =
        calcRating input (fun zeros ones -> if zeros > ones then '0' else '1')

    let co2ScrubberRatingBinary input =
        calcRating input (fun zeros ones -> if zeros > ones then '1' else '0')

    let oxygenGeneratorRating input =
        System.Convert.ToUInt32(oxygenGeneratorRatingBinary input, 2)

    let co2ScrubberRating input =
        System.Convert.ToUInt32(co2ScrubberRatingBinary input, 2)
