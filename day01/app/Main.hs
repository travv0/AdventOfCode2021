{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad (filterM)
import Control.Monad.ST (runST)
import Data.List (tails)
import Data.Maybe (mapMaybe)
import Data.STRef (newSTRef, readSTRef, writeSTRef)

main :: IO ()
main = do
    input <- readInput <$> readFile "input.txt"

    putStrLn $
        "Number of times depth increases: " <> show (countIncreases input)

    putStrLn $
        "Number of times sum in sliding window increases: "
            <> show (countIncreases (sums input))

readInput :: String -> [Integer]
readInput = fmap read . lines

countIncreases :: Ord a => [a] -> Int
countIncreases xs = runST $ do
    prev <- newSTRef $ head xs
    length
        <$> filterM
            ( \x -> do
                p <- readSTRef prev
                writeSTRef prev x
                return (x > p)
            )
            (tail xs)

sumGroup :: [Integer] -> Maybe Integer
sumGroup = \case
    a : b : c : _ -> Just (a + b + c)
    _ -> Nothing

sums :: [Integer] -> [Integer]
sums = mapMaybe sumGroup . tails
