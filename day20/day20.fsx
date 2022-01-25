open System
open System.IO

type Pixel =
    | Light
    | Dark

type Coords = (int * int)

type EnhancementAlg = Pixel []

type Image = Map<Coords, Pixel>

let charToPixel =
    function
    | '#' -> Light
    | '.' -> Dark
    | p -> failwithf "bad pixel %c" p

let parseInput (input: string) : (EnhancementAlg * Image) =
    match input.Split("\n\n") with
    | [| alg; img |] ->
        let algorithm = Seq.map charToPixel alg |> Seq.toArray

        let image =
            Map.ofList [ for y, row in Seq.indexed (img.Split('\n')) do
                             yield ((-1, y), Dark)

                             for x, char in Seq.indexed row do
                                 yield ((x, y), charToPixel char) ]

        (algorithm, image)
    | _ -> failwith "bad parse"

let getBinary def image (x, y) =
    let pixelToBit =
        function
        | Dark -> '0'
        | Light -> '1'

    let makeCoords dx dy = (x + dx, y + dy)

    let coordsGrid =
        [| for y in [ -1; 0; 1 ] do
               for x in [ -1; 0; 1 ] do
                   yield makeCoords x y |]

    let pixelOrDefault c =
        image |> Map.tryFind c |> Option.defaultValue def

    Array.map (pixelOrDefault >> pixelToBit) coordsGrid
    |> String

let getOutputPixel def (alg: EnhancementAlg) image coords =
    let index =
        Convert.ToInt32(getBinary def image coords, 2)

    alg.[index]

type Bounds =
    { MinX: int
      MaxX: int
      MinY: int
      MaxY: int }

let getBounds image =
    Seq.fold
        (fun { MinX = minX
               MaxX = mixX
               MinY = minY
               MaxY = mixY } (x, y) ->
            { MinX = min minX x
              MaxX = max mixX x
              MinY = min minY y
              MaxY = max mixY y })
        { MinX = Int32.MaxValue
          MaxX = Int32.MinValue
          MinY = Int32.MaxValue
          MaxY = Int32.MinValue }
        (Map.keys image)

let relevantCoords
    { MinX = minX
      MaxX = maxX
      MinY = minY
      MaxY = maxY }
    =
    [ for y in [ minY - 1 .. maxY + 1 ] do
          for x in [ minX - 1 .. maxX + 1 ] do
              yield (x, y) ]

let defaultPixel { MinX = minX; MinY = minY } image = Map.find (minX, minY) image

let enhanceImage (alg: EnhancementAlg) image =
    let bounds = getBounds image

    relevantCoords bounds
    |> List.map (fun c -> (c, getOutputPixel (defaultPixel bounds image) alg image c))
    |> Map.ofList

let rec enhanceTimes n (alg: EnhancementAlg) (image: Image) =
    if n <= 0 then
        image
    else
        enhanceImage alg image |> enhanceTimes (n - 1) alg

let countLitPixels image =
    image
    |> Map.toSeq
    |> Seq.filter (snd >> (=) Light)
    |> Seq.length

let (alg, image) =
    File.ReadAllText("input.txt") |> parseInput

let pixelCountAfter n =
    enhanceTimes n alg image |> countLitPixels

pixelCountAfter 2
|> printfn "After enhancing the original input image twice, %d pixels are lit"

pixelCountAfter 50
|> printfn "After enhancing the original input image 50 times, %d pixels are lit"
