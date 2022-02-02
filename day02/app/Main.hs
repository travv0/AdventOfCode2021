module Main where

import           Lib                            ( parseCommands )
import qualified Part1
import qualified Part2

main :: IO ()
main = do
  commands <- parseCommands <$> readFile "input.txt"

  let p1coords = Part1.coords commands
  putStrLn
    $  "Final horizontal position multiplied by final depth for part 1: "
    <> show (Part1.position p1coords * Part1.depth p1coords)

  let p2coords = Part2.coords commands
  putStrLn
    $  "Final horizontal position multiplied by final depth for part 1: "
    <> show (Part2.position p2coords * Part2.depth p2coords)
