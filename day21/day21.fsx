open System
open System.IO

type PlayerNum =
    | One
    | Two

type Player =
    { mutable Position: int
      mutable Score: int }

module Player =
    let make pos = { Position = pos; Score = 0 }

    let move spaces player =
        player.Position <- (player.Position + spaces - 1) % 10 + 1
        player

    let updateScore player =
        player.Score <- player.Score + player.Position
        player

type Game =
    { mutable Die: int
      mutable RollCount: int
      WinningScore: int
      Player: PlayerNum -> Player }

module Game =
    let make winningScore playerOnePos playerTwoPos =
        let playerOne = Player.make playerOnePos
        let playerTwo = Player.make playerTwoPos

        { Die = 1
          RollCount = 0
          WinningScore = winningScore
          Player =
            function
            | One -> playerOne
            | Two -> playerTwo }

    let roll times game =
        seq {
            for _ in 1 .. times do
                yield game.Die
                game.Die <- game.Die % 100 + 1
                game.RollCount <- game.RollCount + 1
        }
        |> Seq.sum

    let playTurn playerNum game =
        let spaces = roll 3 game

        game.Player playerNum
        |> Player.move spaces
        |> Player.updateScore
        |> ignore

        game

    let play game =
        let switch =
            function
            | One -> Two
            | Two -> One

        let mutable currentPlayer = Two

        while (game.Player currentPlayer).Score < game.WinningScore do
            currentPlayer <- switch currentPlayer
            playTurn currentPlayer game |> ignore

        (game.Player(switch currentPlayer).Score, game.RollCount)

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
