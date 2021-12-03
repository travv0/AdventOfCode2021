open System.IO

let input = File.ReadLines("input.txt")

let numLength = input |> Seq.head |> Seq.length

let incrementCount (bit: char) (counts: int []) =
    Array.updateAt (int bit - int '0') (counts.[int bit - int '0'] + 1) counts

module Part1 =
    let countBits (acc: int [] []) (num: char seq) =
        [| for counts, bit in Seq.zip acc num do
               incrementCount bit counts |]

    let counts =
        input
        |> Seq.fold countBits (Array.create numLength (Array.create 2 0))

    let gammaRateBinary =
        [| for [| zeros; ones |] in counts do
               if zeros > ones then '0' else '1' |]
        |> System.String

    let gammaRate =
        System.Convert.ToUInt32(gammaRateBinary, 2)

    let epsilonRateBinary =
        [| for [| zeros; ones |] in counts do
               if zeros < ones then '0' else '1' |]
        |> System.String

    let epsilonRate =
        System.Convert.ToUInt32(epsilonRateBinary, 2)

    printfn "The power consumption of the submarine is %d" (gammaRate * epsilonRate)

module Part2 =
    let countBit index (acc: int []) (num: string) = incrementCount num.[index] acc

    let calcRating rule =
        let mutable result = input

        for i in 0 .. numLength - 1 do
            if Seq.length result > 1 then
                let [| zeros; ones |] =
                    result |> Seq.fold (countBit i) (Array.create 2 0)

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
