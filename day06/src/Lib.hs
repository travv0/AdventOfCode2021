module Lib where

import           Control.Arrow                  ( (&&&) )
import           Data.Char                      ( isSpace )
import           Data.List                      ( group
                                                , sort
                                                )
import           Data.List.Split                ( splitOn )
import           Data.Maybe                     ( fromMaybe )
import           Data.Sequence                  ( Seq((:<|))
                                                , (|>)
                                                )
import qualified Data.Sequence                 as S
import           GHC.Exts                       ( fromList )

type Fish = Integer
type Fishes = Seq Fish

countBy :: (Ord a, Num value) => (a -> key) -> [a] -> [(key, value)]
countBy f = map (f . head &&& fromIntegral . length) . group . sort

parseInput :: String -> Fishes
parseInput input = fromList
  $ map (\i -> fromMaybe 0 . lookup i $ counts) [0 :: Integer .. 8]
 where
  fishNums = splitOn "," $ filter (not . isSpace) input
  counts   = countBy read fishNums

step :: Fishes -> Fishes
step (zeros :<| fishes) | length fishes == 8 =
  S.adjust' (+ zeros) 6 fishes |> zeros
step _ = error "where your fishes at?"

stepTimes :: Int -> Fishes -> Fishes
stepTimes 0 fishes = fishes
stepTimes n fishes = stepTimes (n - 1) (step fishes)
