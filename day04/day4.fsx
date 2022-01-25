open System
open System.IO

type Cell = { Num: uint; Called: bool }

type Board = Cell [,]

type ParseResult =
    { Queue: list<uint>
      Boards: list<Board> }

let parseInput (input: string) : ParseResult =
    let sections =
        input.Split(
            [| "\r\n\r\n"; "\n\n" |],
            StringSplitOptions.RemoveEmptyEntries
            ||| StringSplitOptions.TrimEntries
        )
        |> List.ofSeq

    match sections with
    | first :: rest ->
        let queue =
            first.Split(",")
            |> Seq.map UInt32.Parse
            |> List.ofSeq

        let boards =
            [ for board in rest do
                  let lines =
                      board.Split(
                          [| "\r\n"; "\n" |],
                          StringSplitOptions.RemoveEmptyEntries
                          ||| StringSplitOptions.TrimEntries
                      )

                  array2D [ for line in lines do
                                line.Split(
                                    " ",
                                    StringSplitOptions.RemoveEmptyEntries
                                    ||| StringSplitOptions.TrimEntries
                                )
                                |> Seq.filter ((<>) "")
                                |> Seq.map (fun s ->
                                    { Num = UInt32.Parse(s)
                                      Called = false }) ] ]

        { Queue = queue; Boards = boards }
    | _ -> failwithf "bad parse: %A" sections

let checkForWin (board: Board) =
    let range = seq { 0 .. Seq.length board.[0, *] - 1 }

    range
    |> Seq.exists (fun i ->
        board.[i, *]
        |> Seq.forall (fun cell -> cell.Called))
    || range
       |> Seq.exists (fun i ->
           board.[*, i]
           |> Seq.forall (fun cell -> cell.Called))

let markCell number board =
    board
    |> Array2D.map (fun cell -> { cell with Called = cell.Called || cell.Num = number })

let sumUnmarked board =
    seq { for cell in Seq.cast board -> if not cell.Called then cell.Num else 0u }
    |> Seq.sum

let calcWinningScore lastCalled board = sumUnmarked board * lastCalled

let rec findWinningScore lastCalled queue boards =
    match Seq.tryFind checkForWin boards with
    | Some board -> calcWinningScore lastCalled board
    | None ->
        match queue with
        | calledNum :: newQueue -> findWinningScore calledNum newQueue (Seq.map (markCell calledNum) boards)
        | [] -> failwith "game ended without a winner"

let findFirstWinningScore queue boards = findWinningScore 0u queue boards

let fileName =
    match fsi.CommandLineArgs |> Array.toList with
    | _ :: fn :: _ -> fn
    | _ -> "input.txt"

let { Queue = queue; Boards = boards } =
    File.ReadAllText(fileName) |> parseInput

findFirstWinningScore queue boards
|> printfn "Score of first card to win: %d"

let findLastWinningScore queue boards =
    let rec findLastWinningScore' queue boards =
        let newBoards =
            boards |> List.filter (not << checkForWin)

        match newBoards, queue with
        | [ board ], calledNum :: newQueue -> findWinningScore calledNum newQueue [ markCell calledNum board ]
        | [], _ -> failwith "ran out of boards"
        | _, calledNum :: newQueue -> findLastWinningScore' newQueue (List.map (markCell calledNum) newBoards)
        | _, [] -> failwith "game ended without a winner"

    findLastWinningScore' queue boards

findLastWinningScore queue boards
|> printfn "Score of last card to win: %d"
