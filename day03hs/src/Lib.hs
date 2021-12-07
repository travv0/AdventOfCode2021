{-# LANGUAGE NamedFieldPuns #-}

module Lib where

import Data.Char (digitToInt)
import Data.Foldable (foldl', toList)
import Data.Maybe (listToMaybe)
import Data.Vector (Vector, (!))
import GHC.Exts (fromList)
import Numeric (readInt)

data Count = Count {zeros :: Int, ones :: Int}
    deriving (Show)

instance Semigroup Count where
    Count{zeros = za, ones = oa} <> Count{zeros = zb, ones = ob} =
        Count{zeros = za + zb, ones = oa + ob}

instance Monoid Count where
    mempty = Count{zeros = 0, ones = 0}

readInput :: FilePath -> IO [String]
readInput fileName = lines <$> readFile fileName

calcCounts :: [String] -> [Count]
calcCounts input = foldl' countBits (replicate len mempty) input
  where
    len = length $ head input

incrementCount :: Count -> Char -> Count
incrementCount count bit =
    case bit of
        '0' -> count <> mempty{zeros = 1}
        '1' -> count <> mempty{ones = 1}
        _ -> error $ "Bad input bit " <> show bit

countBits :: [Count] -> String -> [Count]
countBits = zipWith incrementCount

calcRate :: (Int -> Int -> Bool) -> [Count] -> Maybe Integer
calcRate comp =
    readBinary
        . map
            ( \Count{zeros, ones} ->
                if zeros `comp` ones then '0' else '1'
            )

gammaRate :: [Count] -> Maybe Integer
gammaRate = calcRate (>)

epsilonRate :: [Count] -> Maybe Integer
epsilonRate = calcRate (<)

readBinary :: String -> Maybe Integer
readBinary = fmap fst . listToMaybe . readInt 2 (`elem` "01") digitToInt

countBit :: Int -> Count -> Vector Char -> Count
countBit index acc num = incrementCount acc (num ! index)

calcRating :: (Int -> Int -> Char) -> [String] -> Maybe Integer
calcRating rule input =
    readBinary $ go 0 (map fromList input)
  where
    go _ [num] = toList num
    go _ [] = error "Ran out of bits"
    go i result =
        let Count{zeros, ones} = foldl' (countBit i) mempty result
            next = filter (\num -> num ! i == rule zeros ones) result
         in go (i + 1) next

oxygenGeneratorRating :: [String] -> Maybe Integer
oxygenGeneratorRating =
    calcRating (\zeros ones -> if zeros > ones then '0' else '1')

co2ScrubberRating :: [String] -> Maybe Integer
co2ScrubberRating =
    calcRating (\zeros ones -> if zeros > ones then '1' else '0')