#!/usr/bin/env cabal
{- cabal:
ghc-options: -Wall
build-depends: base, containers, hashtables, hashable
-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Strict #-}

import           Control.Monad.ST               ( ST
                                                , runST
                                                )
import qualified Data.HashTable.ST.Basic       as HT
import           Data.Hashable                  ( Hashable )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           GHC.Exts                       ( fromList )
import           GHC.Generics                   ( Generic )

data PlayerNum =  One | Two deriving (Show, Eq, Ord, Generic)

instance Hashable PlayerNum

switchPlayer :: PlayerNum -> PlayerNum
switchPlayer One = Two
switchPlayer Two = One

data Player = Player
    { playerPos   :: Int
    , playerScore :: Int
    }
    deriving Show

makePlayer :: Int -> Player
makePlayer pos = Player { playerPos = pos, playerScore = 0 }

move :: Int -> Player -> Player
move spaces player =
    player { playerPos = (playerPos player + spaces - 1) `mod` 10 + 1 }

updateScore :: Player -> Player
updateScore player =
    player { playerScore = playerScore player + playerPos player }

data Game = Game
    { gameDie          :: Int
    , gameRollCount    :: Int
    , gameWinningScore :: Int
    , gamePlayers      :: Map PlayerNum Player
    }
    deriving Show

data QuantumWins = Wins
    { qwPlayerOne :: Integer
    , qwPlayerTwo :: Integer
    }
    deriving Show

instance Semigroup QuantumWins where
    Wins { qwPlayerOne = playerOne1, qwPlayerTwo = playerTwo1 } <> Wins { qwPlayerOne = playerOne2, qwPlayerTwo = playerTwo2 }
        = Wins { qwPlayerOne = playerOne1 + playerOne2
               , qwPlayerTwo = playerTwo1 + playerTwo2
               }

instance Monoid QuantumWins where
    mempty = Wins { qwPlayerOne = 0, qwPlayerTwo = 0 }

maxWin :: QuantumWins -> Integer
maxWin Wins { qwPlayerOne = playerOne, qwPlayerTwo = playerTwo } =
    max playerOne playerTwo

makeGame :: Int -> Int -> Int -> Game
makeGame winningScore playerOnePos playerTwoPos =
    let playerOne = makePlayer playerOnePos
        playerTwo = makePlayer playerTwoPos
    in  Game { gameDie          = 100
             , gameRollCount    = 0
             , gameWinningScore = winningScore
             , gamePlayers      = fromList [(One, playerOne), (Two, playerTwo)]
             }

updatePlayer :: PlayerNum -> Player -> Game -> Game
updatePlayer playerNum player game =
    game { gamePlayers = Map.insert playerNum player (gamePlayers game) }

getPlayer :: PlayerNum -> Game -> Player
getPlayer playerNum game = gamePlayers game Map.! playerNum

alterPlayer :: PlayerNum -> (Player -> Player) -> Game -> Game
alterPlayer playerNum f game =
    game { gamePlayers = Map.alter (fmap f) playerNum (gamePlayers game) }

rollDeterministic :: Int -> Game -> (Int, Game)
rollDeterministic times game =
    let xs = [ (gameDie game + i - 1) `mod` 100 + 1 | i <- [1 .. times] ]
    in  ( sum xs
        , game { gameDie = last xs, gameRollCount = gameRollCount game + times }
        )

playDeterministicTurn :: PlayerNum -> Game -> Game
playDeterministicTurn playerNum game =
    let (spaces, game') = rollDeterministic 3 game
    in  alterPlayer playerNum (updateScore . move spaces) game'

playDeterministic :: Game -> (Int, Int)
playDeterministic = loop One
  where
    loop currentPlayer game =
        let game'       = playDeterministicTurn currentPlayer game
            player      = getPlayer currentPlayer game'
            otherPlayer = getPlayer (switchPlayer currentPlayer) game'
        in  if playerScore player >= gameWinningScore game'
                then (playerScore otherPlayer, gameRollCount game')
                else loop (switchPlayer currentPlayer) game'

type HashTable s k v = HT.HashTable s k v

type Key = (Int, PlayerNum, Int, Int, Int, Int)
type WinCache s = HashTable s Key QuantumWins

makeKey :: Game -> PlayerNum -> (Int, PlayerNum, Int, Int, Int, Int)
makeKey game playerNum =
    let playerOne = getPlayer One game
        playerTwo = getPlayer Two game
    in  ( gameWinningScore game
        , playerNum
        , playerPos playerOne
        , playerScore playerOne
        , playerPos playerTwo
        , playerScore playerTwo
        )

cacheHit :: WinCache s -> Game -> PlayerNum -> ST s (Maybe QuantumWins)
cacheHit winCache game playerNum =
    let key = makeKey game playerNum in HT.lookup winCache key

cacheWins :: WinCache s -> Game -> PlayerNum -> QuantumWins -> ST s QuantumWins
cacheWins winCache game playerNum wins = do
    let key = makeKey game playerNum
    r <- HT.lookup winCache key
    case r of
        Just w  -> return w
        Nothing -> do
            HT.insert winCache key wins
            return wins

playQuantum :: Game -> QuantumWins
playQuantum game = runST $ do
    winCache <- HT.new
    playQuantum' winCache Two Nothing game

playQuantum'
    :: WinCache s -> PlayerNum -> Maybe Int -> Game -> ST s QuantumWins
playQuantum' winCache playerNum spaces game = do
    let (player, game') = case spaces of
            Just spc ->
                let player' = updateScore . move spc $ getPlayer playerNum game
                in  (player', updatePlayer playerNum player' game)
            Nothing -> (getPlayer playerNum game, game)
    hit <- cacheHit winCache game' playerNum
    case hit of
        Just wins -> return wins
        Nothing   -> if playerScore player < gameWinningScore game'
            then
                do
                    rollQuantum winCache game' (switchPlayer playerNum) 3 0
                >>= cacheWins winCache game' playerNum
            else case playerNum of
                One -> return Wins { qwPlayerOne = 1, qwPlayerTwo = 0 }
                Two -> return Wins { qwPlayerOne = 0, qwPlayerTwo = 1 }

rollQuantum
    :: WinCache s -> Game -> PlayerNum -> Int -> Int -> ST s QuantumWins
rollQuantum winCache game playerNum n acc = if n > 0
    then foldMap
        (\i -> rollQuantum winCache game playerNum (n - 1) (acc + i))
        [1 .. 3]
    else playQuantum' winCache playerNum (Just acc) game

calcPart1 :: Int -> Int -> Int
calcPart1 playerOnePos playerTwoPos =
    let (losingScore, rollCount) =
            playDeterministic $ makeGame 1000 playerOnePos playerTwoPos
    in  losingScore * rollCount

calcPart2 :: Int -> Int -> Integer
calcPart2 playerOnePos playerTwoPos =
    maxWin . playQuantum $ makeGame 21 playerOnePos playerTwoPos

parseInput :: String -> (Int, Int)
parseInput input = case lines input of
    [one, two] -> (read . last $ words one, read . last $ words two)
    _          -> error "bad parse"

main :: IO ()
main = do
    (playerOnePos, playerTwoPos) <- parseInput <$> readFile "input.txt"

    putStrLn
        $ "The score of the losing player multiplied by the number of times the die was rolled is "
        <> show (calcPart1 playerOnePos playerTwoPos)

    putStrLn
        $  "The player that wins more with the quantum die wins in "
        <> show (calcPart2 playerOnePos playerTwoPos)
        <> " universes"
