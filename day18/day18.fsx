open System
open System.IO

let fileName =
    fsi.CommandLineArgs
    |> Array.skip 1
    |> Array.tryHead
    |> Option.defaultValue "input.txt"

let input = File.ReadAllText(fileName)

type SnailfishNumber =
    | Number of int
    | Pair of SnailfishNumber * SnailfishNumber

    override this.ToString() =
        match this with
        | Pair (a, b) -> sprintf "[%s,%s]" (a.ToString()) (b.ToString())
        | Number n -> n.ToString()

module SnailfishNumber =
    type Dir =
        | L
        | R

    let rec addToLeft num i =
        match num with
        | Pair (a, b) -> Pair(addToLeft a i, b)
        | Number n -> Number(n + i)

    let rec addToRight num i =
        match num with
        | Pair (a, b) -> Pair(a, addToRight b i)
        | Number n -> Number(n + i)

    let rec edgeExplodeLeft =
        function
        | Pair (Pair (Number _, Number b), c) -> Pair(Number 0, addToLeft c b)
        | Pair (a, b) -> Pair(edgeExplodeLeft a, b)
        | Number _ -> failwith "edgeExplodeLeft: hit dead end"

    let rec edgeExplodeRight =
        function
        | Pair (a, Pair (Number b, Number _)) -> Pair(addToRight a b, Number 0)
        | Pair (a, b) -> Pair(a, edgeExplodeRight b)
        | Number _ -> failwith "edgeExplodeRight: hit dead end"

    let rec walkPath num path =
        match (path, num) with
        | [], _ -> num
        | L :: path, Pair (a, _) -> walkPath a path
        | R :: path, Pair (_, b) -> walkPath b path
        | _, Number _ -> failwith "walkPath: hit number"

    let rec explodeRight num path =
        match num with
        | Pair (a, b) ->
            match walkPath num path with
            | Pair (Number n, Number _) ->
                Pair(addToRight a n, edgeExplodeLeft b)
            | _ -> failwith "explodeRight: path didn't lead to pair of numbers"
        | Number _ -> failwith "explodeRight: hit number"

    let rec explodeLeft num path =
        match num with
        | Pair (a, b) ->
            match walkPath num path with
            | Pair (Number _, Number m) ->
                Pair(edgeExplodeRight a, addToLeft b m)
            | _ -> failwith "explodeLeft: path didn't lead to pair of numbers"
        | Number _ -> failwith "explodeLeft: hit number"

    let rec explode num path =
        if List.forall ((=) L) path then
            edgeExplodeLeft num
        else if List.forall ((=) R) path then
            edgeExplodeRight num
        else
            match path with
            | dir :: tl ->
                if tl |> List.forall ((=) L) then
                    explodeRight num path
                else if tl |> List.forall ((=) R) then
                    explodeLeft num path
                else
                    match (dir, num) with
                    | L, Pair (a, b) -> Pair(explode a tl, b)
                    | R, Pair (a, b) -> Pair(a, explode b tl)
                    | _, _ ->
                        failwithf
                            "explode: bad path for num %s"
                            (num.ToString())
            | _ -> failwith "explode: empty path"

    let rec explodePath num path =
        match num with
        | Pair (a, b) ->
            if List.length path >= 4 then
                List.rev path
            else
                let leftPath = explodePath a (L :: path)

                if List.length leftPath >= 4 then
                    leftPath
                else
                    let rightPath = explodePath b (R :: path)

                    if List.length rightPath >= 4 then
                        rightPath
                    else
                        []
        | Number _ -> []

    let rec split n =
        Pair(Number(n / 2), Number(double n / 2. |> ceil |> int))

    let rec maybeSplit =
        function
        | Pair (a, b) as pair ->
            let num, splitDone = maybeSplit a

            if splitDone then
                (Pair(num, b), true)
            else
                let num, splitDone = maybeSplit b

                if splitDone then
                    (Pair(a, num), true)
                else
                    (pair, false)
        | Number n as num ->
            if n >= 10 then
                (split n, true)
            else
                (num, false)

    let rec reduce num =
        let path = explodePath num []

        if List.length path > 0 then
            explode num path |> reduce
        else
            let newNum, splitDone = maybeSplit num
            if splitDone then reduce newNum else num

    let rec magnitude =
        function
        | Number n -> n
        | Pair (a, b) -> 3 * magnitude a + 2 * magnitude b

type SnailfishNumber with
    member this.Magnitude = SnailfishNumber.magnitude this

    static member Parse s =
        let parseNum cs =
            let digits, remaining =
                List.takeWhile Char.IsDigit cs, List.skipWhile Char.IsDigit cs

            (Number(digits |> Array.ofSeq |> String |> int), remaining)

        let rec parsePair (cs: char list) =
            let firstElem, cs = parseElement cs
            let cs = List.skipWhile ((=) ',') cs
            let secondElem, cs = parseElement cs

            let cs =
                match cs with
                | ']' :: cs -> cs
                | c :: _ ->
                    failwithf "parsePair: no closing bracket, found '%c'" c
                | [] -> failwith "parsePair: hit end of string"

            (Pair(firstElem, secondElem), cs)

        and parseElement (cs: char list) =
            match cs with
            | '[' :: cs -> parsePair cs
            | _ -> parseNum cs

        s |> Seq.toList |> parseElement |> fst

    member this.Reduced = SnailfishNumber.reduce this

    static member (+)(num1, num2) = Pair(num1, num2).Reduced

let nums =
    input.Split(
        '\n',
        StringSplitOptions.RemoveEmptyEntries
        ||| StringSplitOptions.TrimEntries
    )
    |> Seq.map SnailfishNumber.Parse
    |> Seq.toList

nums
|> List.reduce (+)
|> SnailfishNumber.magnitude
|> printfn "The magnitude of the final sum is %d"

nums
|> Seq.map
    (fun n ->
        nums
        |> List.choose
            (fun m ->
                if n <> m then
                    Some((n + m).Magnitude)
                else
                    None))
|> Seq.concat
|> Seq.max
|> printfn
    "The largest magnitude of any sum of two different snailfish numbers \
    from the homework assignment is %d"
