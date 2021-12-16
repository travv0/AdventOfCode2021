module Lib where

import Data.Char (isNumber, isSpace)
import Data.Sequence (Seq ((:<|)), (|>))
import qualified Data.Sequence as S
import GHC.Exts (fromList)

type Fish = Integer
type Fishes = Seq Fish

split :: Eq a => a -> [a] -> [[a]]
split c s = case rest of
    [] -> [chunk]
    _ : rest -> chunk : split c rest
  where
    (chunk, rest) = break (== c) s

parseInput :: String -> Fishes
parseInput input =
    fromList $
        fromIntegral . length
            <$> ( filter
                    <$> map ((==) . head . show) [0 .. 8]
                    <*> [fishNums]
                )
  where
    fishNums = filter isNumber input

step :: Fishes -> Fishes
step (zeros :<| fishes) = S.adjust (+ zeros) 6 fishes |> zeros
step _ = error "where your fishes at?"

stepTimes :: Int -> Fishes -> Fishes
stepTimes 0 fishes = fishes
stepTimes n fishes = stepTimes (n - 1) (step fishes)