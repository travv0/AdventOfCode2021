{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Foldable (foldl')
import Data.Maybe (mapMaybe)

main :: IO ()
main = do
    commands <- parseCommands <$> readFile "input.txt"

    let p1coords = part1Coords commands
    putStrLn $
        "Final horizontal position multiplied by final depth for part 1: "
            <> show (p1Position p1coords * p1Depth p1coords)

    let p2coords = part2Coords commands
    putStrLn $
        "Final horizontal position multiplied by final depth for part 2: "
            <> show (p2Position p2coords * p2Depth p2coords)

data Command = Forward | Down | Up

parseCommand :: [Char] -> Command
parseCommand = \case
    "forward" -> Forward
    "down" -> Down
    "up" -> Up
    command -> error $ "Invalid command: " <> command

parseLine :: String -> Maybe (Command, Integer)
parseLine line =
    case words line of
        [command, units] -> Just (parseCommand command, read units)
        _ -> Nothing

parseCommands :: String -> [(Command, Integer)]
parseCommands = mapMaybe parseLine . lines

data Part1Coords = Part1Coords {p1Depth :: Integer, p1Position :: Integer}

part1MoveSub :: Part1Coords -> (Command, Integer) -> Part1Coords
part1MoveSub coords (command, units) =
    case command of
        Forward -> coords{p1Position = p1Position coords + units}
        Down -> coords{p1Depth = p1Depth coords + units}
        Up -> coords{p1Depth = p1Depth coords - units}

part1Coords :: [(Command, Integer)] -> Part1Coords
part1Coords =
    foldl' part1MoveSub Part1Coords{p1Position = 0, p1Depth = 0}

data Part2Coords = Part2Coords
    {p2Depth :: Integer, p2Position :: Integer, p2Aim :: Integer}

part2MoveSub :: Part2Coords -> (Command, Integer) -> Part2Coords
part2MoveSub coords (command, units) =
    case command of
        Forward ->
            coords
                { p2Position = p2Position coords + units
                , p2Depth = p2Depth coords + p2Aim coords * units
                }
        Down -> coords{p2Aim = p2Aim coords + units}
        Up -> coords{p2Aim = p2Aim coords - units}

part2Coords :: [(Command, Integer)] -> Part2Coords
part2Coords =
    foldl' part2MoveSub Part2Coords{p2Position = 0, p2Depth = 0, p2Aim = 0}
