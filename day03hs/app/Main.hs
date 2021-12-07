module Main where

import Control.Applicative (liftA2)
import Lib
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    let fileName = case args of
            fn : _ -> fn
            _ -> "input.txt"

    input <- readInput fileName
    let counts = calcCounts input

    let (Just part1answer) = liftA2 (*) (gammaRate counts) (epsilonRate counts)
    let (Just part2answer) = liftA2 (*) (oxygenGeneratorRating input) (co2ScrubberRating input)

    putStrLn $ "The power consumption of the submarine is " <> show part1answer
    putStrLn $ "The life support rating of the submarine is " <> show part2answer