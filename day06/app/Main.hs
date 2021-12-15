module Main where

import Lib

main :: IO ()
main = do
    fishes <- parseInput <$> readFile "input.txt"
    putStrLn $ "Number of lanternfish after 80 days: " <> show (sum (stepTimes 80 fishes))
    putStrLn $ "Number of lanternfish after 256 days: " <> show (sum (stepTimes 256 fishes))