{-# LANGUAGE LambdaCase #-}

module Lib where

import Data.Maybe (mapMaybe)

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
