open System.Collections.Generic
open System.IO

type PlayerNum =
    | One
    | Two

module PlayerNum =
    let switch =
        function
        | One -> Two
        | Two -> One

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

type QuantumWins =
    { PlayerOne: int64
      PlayerTwo: int64 }
    static member (+)
        (
            { PlayerOne = playerOne1
              PlayerTwo = playerTwo1 },
            { PlayerOne = playerOne2
              PlayerTwo = playerTwo2 }
        ) =
        { PlayerOne = playerOne1 + playerOne2
          PlayerTwo = playerTwo1 + playerTwo2 }

    static member Zero = { PlayerOne = 0; PlayerTwo = 0 }

    static member Max
        { PlayerOne = playerOne
          PlayerTwo = playerTwo }
        =
        max playerOne playerTwo

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

    let updatePlayer playerNum player game : Game =
        { game with Players = Map.add playerNum player game.Players }

    let getPlayer playerNum game : Player = Map.find playerNum game.Players

    let alterPlayer playerNum f game : Game =
        { game with Players = Map.change playerNum (Option.map f) game.Players }

    module Deterministic =
        let roll times game =
            [ for i in 1 .. times -> (game.Die + i - 1) % 100 + 1 ]
            |> (fun x ->
                List.sum x,
                { game with
                    Die = List.last x
                    RollCount = game.RollCount + times })

        let playTurn playerNum game =
            let (spaces, game) = roll 3 game
            alterPlayer playerNum (Player.move spaces >> Player.updateScore) game

        let play game =
            let rec loop currentPlayer game =
                let game = playTurn currentPlayer game
                let player = getPlayer currentPlayer game

                let otherPlayer =
                    getPlayer (PlayerNum.switch currentPlayer) game

                if player.Score >= game.WinningScore then
                    (otherPlayer.Score, game.RollCount)
                else
                    loop (PlayerNum.switch currentPlayer) game

            loop One game

    module Quantum =
        module private Cache =
            type private Key = (int * PlayerNum * int * int * int * int)

            let private winCache = Dictionary<Key, QuantumWins>()

            let private makeKey (game: Game) playerNum : Key =
                let playerOne = getPlayer One game
                let playerTwo = getPlayer Two game
                (game.WinningScore, playerNum, playerOne.Position, playerOne.Score, playerTwo.Position, playerTwo.Score)

            let cacheHit (game: Game) playerNum : option<QuantumWins> =
                let key = makeKey game playerNum

                if winCache.ContainsKey(key) then
                    Some winCache.[key]
                else
                    None

            let cacheWins game playerNum wins : QuantumWins =
                let key = makeKey game playerNum

                if not <| winCache.ContainsKey(key) then
                    winCache.Add(key, wins)

                wins

        let rec roll (game: Game) (playerNum: PlayerNum) (n: int) (sum: int) : QuantumWins =
            if n > 0 then
                [ for i in 1 .. 3 -> roll game playerNum (n - 1) (sum + i) ]
                |> List.sum
            else
                playTurn game playerNum (Some sum)

        and playTurn (game: Game) (playerNum: PlayerNum) (spaces: option<int>) =
            let player, game =
                match spaces with
                | Some spaces ->
                    let player =
                        getPlayer playerNum game
                        |> Player.move spaces
                        |> Player.updateScore

                    player, updatePlayer playerNum player game
                | None -> getPlayer playerNum game, game

            match Cache.cacheHit game playerNum with
            | Some wins -> wins
            | None ->
                if player.Score < game.WinningScore then
                    roll game (PlayerNum.switch playerNum) 3 0
                    |> Cache.cacheWins game playerNum
                else
                    match playerNum with
                    | One -> { PlayerOne = 1; PlayerTwo = 0 }
                    | Two -> { PlayerOne = 0; PlayerTwo = 1 }

        let play (game: Game) : QuantumWins = playTurn game Two None

let calcPart1 playerOnePos playerTwoPos =
    let (losingScore, rollCount) =
        Game.make 1000 playerOnePos playerTwoPos
        |> Game.Deterministic.play

    losingScore * rollCount

let calcPart2 playerOnePos playerTwoPos =
    Game.make 21 playerOnePos playerTwoPos
    |> Game.Quantum.play
    |> QuantumWins.Max

let parseInput (input: string) =
    match input.Trim().Split('\n') with
    | [| one; two |] -> (one.Split(' ') |> Array.last |> int, two.Split(' ') |> Array.last |> int)
    | _ -> failwith "bad parse"

let playerOnePos, playerTwoPos =
    File.ReadAllText("input.txt") |> parseInput

calcPart1 playerOnePos playerTwoPos
|> printfn "The score of the losing player multiplied by the number of times the die was rolled is %d"

calcPart2 playerOnePos playerTwoPos
|> printfn "The player that wins more with the quantum die wins in %d universes"
