open System
open System.IO

let fileName =
    match fsi.CommandLineArgs |> Array.toList with
    | _ :: fn :: _ -> fn
    | _ -> "input.txt"

let input = File.ReadAllText(fileName)

type Bit =
    | Zero
    | One

type Binary = list<Bit>

module Binary =
    let private charToBit =
        function
        | '0' -> Zero
        | '1' -> One
        | c -> failwithf "bad bit: %c" c

    let private bitToChar =
        function
        | Zero -> '0'
        | One -> '1'

    let private hexDigitToBinary =
        function
        | '0' -> "0000"
        | '1' -> "0001"
        | '2' -> "0010"
        | '3' -> "0011"
        | '4' -> "0100"
        | '5' -> "0101"
        | '6' -> "0110"
        | '7' -> "0111"
        | '8' -> "1000"
        | '9' -> "1001"
        | 'A'
        | 'a' -> "1010"
        | 'B'
        | 'b' -> "1011"
        | 'C'
        | 'c' -> "1100"
        | 'D'
        | 'd' -> "1101"
        | 'E'
        | 'e' -> "1110"
        | 'F'
        | 'f' -> "1111"
        | c -> failwithf "bad hex: %c" c

    let toString binary =
        binary
        |> List.map bitToChar
        |> List.toArray
        |> String

    let ofHexadecimal hex : Binary =
        hex
        |> Seq.map hexDigitToBinary
        |> Seq.concat
        |> Seq.map charToBit
        |> Seq.toList

    let toInt64 (binary: Binary) = Convert.ToInt64(toString binary, 2)

type PacketData =
    | Sum of list<Packet>
    | Product of list<Packet>
    | Minimum of list<Packet>
    | Maximum of list<Packet>
    | Literal of int64
    | GreaterThan of (Packet * Packet)
    | LessThan of (Packet * Packet)
    | EqualTo of (Packet * Packet)

and Packet = { Version: int64; Data: PacketData }

module Packet =
    let rec value packet : int64 =
        match packet.Data with
        | Sum subPackets -> subPackets |> List.map value |> List.sum
        | Product subPackets -> subPackets |> List.map value |> List.fold (*) 1
        | Minimum subPackets -> subPackets |> List.map value |> List.min
        | Maximum subPackets -> subPackets |> List.map value |> List.max
        | Literal i -> i
        | GreaterThan (a, b) -> if value a > value b then 1 else 0
        | LessThan (a, b) -> if value a < value b then 1 else 0
        | EqualTo (a, b) -> if value a = value b then 1 else 0

    let private parseLiteral (binary: Binary) : int64 * Binary =
        let rec parseLiteral' num =
            function
            | checkBit :: binary ->
                let chunk, remaining = List.splitAt 4 binary

                match checkBit with
                | Zero -> (Binary.toInt64 (num @ chunk), remaining)
                | One -> parseLiteral' (num @ chunk) remaining
            | _ -> failwith "parseLiteral': empty binary"

        parseLiteral' [] binary

    let rec private parseMany (binary: Binary) : list<Packet> * Binary =
        match binary with
        | Zero :: binary ->
            let lengthBin, binary = List.splitAt 15 binary
            let length = Binary.toInt64 lengthBin
            let binary, remaining = List.splitAt (int length) binary

            let rec parsePackets packets binary =
                match parsePacket binary with
                | packet, [] -> List.rev (packet :: packets)
                | packet, binary -> parsePackets (packet :: packets) binary

            (parsePackets [] binary, remaining)
        | One :: binary ->
            let lengthBin, binary = List.splitAt 11 binary
            let subPacketCount = Binary.toInt64 lengthBin

            let rec parsePackets n packets binary =
                let packet, binary = parsePacket binary

                if n <= 1L then
                    (List.rev (packet :: packets), binary)
                else
                    parsePackets (n - 1L) (packet :: packets) binary

            parsePackets subPacketCount [] binary
        | _ -> failwith "parseMany: empty binary"

    and private parsePacket (binary: Binary) : Packet * Binary =
        let parseTwo =
            parseMany
            >> function
                | [ a; b ], c -> ((a, b), c)
                | l, _ ->
                    failwithf
                        "parseTwo: invalid number of elements: %d"
                        (List.length l)

        let v, binary = List.splitAt 3 binary
        let version = Binary.toInt64 v
        let type_, binary = List.splitAt 3 binary

        let data, remaining =
            match Binary.toInt64 type_ with
            | 0L ->
                let packet, remaining = parseMany binary in

                (Sum packet, remaining)
            | 1L ->
                let packet, remaining = parseMany binary in

                (Product packet, remaining)
            | 2L ->
                let packet, remaining = parseMany binary in

                (Minimum packet, remaining)
            | 3L ->
                let packet, remaining = parseMany binary in

                (Maximum packet, remaining)
            | 4L ->
                let literal, remaining = parseLiteral binary in

                (Literal literal, remaining)
            | 5L ->
                let packet, remaining = parseTwo binary in

                (GreaterThan packet, remaining)
            | 6L ->
                let packet, remaining = parseTwo binary in

                (LessThan packet, remaining)
            | 7L ->
                let packet, remaining = parseTwo binary in

                (EqualTo packet, remaining)
            | _ -> failwith "parsePacket: bad packet"

        ({ Version = version; Data = data }, remaining)

    let parse (s: string) : Packet =
        s.Trim()
        |> Binary.ofHexadecimal
        |> parsePacket
        |> fst

    let rec sumVersions packet =
        packet.Version
        + match packet.Data with
          | Sum op
          | Product op
          | Minimum op
          | Maximum op -> List.map sumVersions op |> List.sum
          | GreaterThan (a, b)
          | LessThan (a, b)
          | EqualTo (a, b) -> sumVersions a + sumVersions b
          | _ -> 0L

let packet = Packet.parse input

Packet.sumVersions packet
|> printfn "The version sum of the package hierarchy is %d"

Packet.value packet
|> printfn "The value of the transmission is %d"
