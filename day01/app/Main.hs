module Main where

import           Data.List                      ( foldl'
                                                , tails
                                                )
import           Data.Maybe                     ( mapMaybe )

main :: IO ()
main = do
  input <- readInput <$> readFile "input.txt"
  putStrLn $ "Number of times depth increases: " <> show (countIncreases input)
  putStrLn $ "Number of times sum in sliding window increases: " <> show
    (countIncreases (sums input))

readInput :: String -> [Integer]
readInput = fmap read . lines

countIncreases :: Ord a => [a] -> Int
countIncreases []          = error "countIncreases: empty list"
countIncreases (prev : xs) = length reduced - 1
 where
  reduced = foldl' (\l@(p : r) x -> if x > p then x : l else x : r) [prev] xs

sumGroup :: [Integer] -> Maybe Integer
sumGroup (a : b : c : _) = Just (a + b + c)
sumGroup _               = Nothing

sums :: [Integer] -> [Integer]
sums = mapMaybe sumGroup . tails
