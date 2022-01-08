open System
open System.IO

type PlayerNum =
    | One
    | Two

type Player = { Position: int; Score: int }

module Player =
    let make pos = { Position = pos; Score = 0 }

    let move spaces player =
        { player with Position = (player.Position + spaces - 1) % 10 + 1 }

    let updateScore player =
        { player with Score = player.Score + player.Position }

type Game =
    { Die: int
      RollCount: int
      WinningScore: int
      Players: Map<PlayerNum, Player> }

module Game =
    let make winningScore playerOnePos playerTwoPos =
        let playerOne = Player.make playerOnePos
        let playerTwo = Player.make playerTwoPos

        { Die = 100
          RollCount = 0
          WinningScore = winningScore
          Players =
            Map.ofList [ (One, playerOne)
                         (Two, playerTwo) ] }

    let roll times game =
        [ for i in 1 .. times -> (game.Die + i - 1) % 100 + 1 ]
        |> (fun x ->
            List.sum x,
            { game with
                Die = List.last x
                RollCount = game.RollCount + times })

    let playTurn playerNum game =
        let (spaces, game) = roll 3 game
        { game with Players = Map.change playerNum (Option.map (Player.move spaces >> Player.updateScore)) game.Players }


    let play game =
        let switch =
            function
            | One -> Two
            | Two -> One

        let rec loop currentPlayer game =
            let game = playTurn currentPlayer game

            if (Map.find currentPlayer game.Players).Score
               >= game.WinningScore then
                ((Map.find (switch currentPlayer) game.Players)
                    .Score,
                 game.RollCount)
            else
                loop (switch currentPlayer) game

        loop One game

let calcPart1 playerOnePos playerTwoPos =
    let (losingScore, rollCount) =
        Game.make 1000 playerOnePos playerTwoPos
        |> Game.play

    losingScore * rollCount

let parseInput (input: string) =
    match input.Trim().Split('\n') with
    | [| one; two |] -> (one.Split(' ') |> Array.last |> int, two.Split(' ') |> Array.last |> int)
    | _ -> failwith "bad parse"

File.ReadAllText("input.txt")
|> parseInput
||> calcPart1
|> printfn "The score of the losing player multiplied by the number of times the die was rolled is %d"
