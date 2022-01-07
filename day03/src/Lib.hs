{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Lib
  ( parseCounts
  , gammaRate
  , epsilonRate
  , co2ScrubberRating
  , oxygenGeneratorRating
  ) where

import           Data.Char                      ( digitToInt )
import           Data.Foldable                  ( foldl' )

data Count = Count
  { zeros :: Int
  , ones  :: Int
  }

instance Semigroup  Count where
  Count { zeros = zeros1, ones = ones1 } <> Count { zeros = zeros2, ones = ones2 }
    = Count { zeros = zeros1 + zeros2, ones = ones1 + ones2 }

instance Monoid Count where
  mempty = Count { zeros = 0, ones = 0 }

incrementCount :: Count -> Char -> Count
incrementCount count = \case
  '0' -> count <> mempty { zeros = 1 }
  '1' -> count <> mempty { ones = 1 }
  bit -> error $ "Bad input bit " <> show bit

countBits :: [Count] -> String -> [Count]
countBits = zipWith incrementCount

parseCounts :: [String] -> [Count]
parseCounts input =
  let numLength = length $ head input
  in  foldl' countBits (replicate numLength mempty) input

fromBase2 :: String -> Int
fromBase2 = foldl' (\acc x -> acc * 2 + digitToInt x) 0

calcRate :: (Int -> Int -> Bool) -> [Count] -> Int
calcRate comp counts =
  let binary = map
        (\Count { zeros, ones } -> if comp zeros ones then '0' else '1')
        counts
  in  fromBase2 binary

gammaRate :: [Count] -> Int
gammaRate = calcRate (>)

epsilonRate :: [Count] -> Int
epsilonRate = calcRate (<)

countBit :: Int -> Count -> [Char] -> Count
countBit index acc num = incrementCount acc (num !! index)

calcRating :: (Int -> Int -> Char) -> [String] -> Int
calcRating rule input = go input 0
 where
  go result i
    | length result > 1
    = let Count { zeros, ones } = foldl' (countBit i) mempty result
      in  go (filter (\num -> num !! i == rule zeros ones) result) (i + 1)
    | otherwise
    = fromBase2 . head $ result

oxygenGeneratorRating :: [String] -> Int
oxygenGeneratorRating =
  calcRating (\zeros ones -> if zeros > ones then '0' else '1')

co2ScrubberRating :: [String] -> Int
co2ScrubberRating =
  calcRating (\zeros ones -> if zeros > ones then '1' else '0')

