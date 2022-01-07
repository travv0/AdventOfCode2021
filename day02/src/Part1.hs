module Part1 where

import           Data.Foldable                  ( foldl' )
import           Lib                            ( Command(..) )

data Coords = Coords
  { depth    :: Integer
  , position :: Integer
  }

moveSub :: Coords -> (Command, Integer) -> Coords
moveSub cs (command, units) = case command of
  Forward -> cs { position = position cs + units }
  Down    -> cs { depth = depth cs + units }
  Up      -> cs { depth = depth cs - units }

coords :: [(Command, Integer)] -> Coords
coords = foldl' moveSub Coords { position = 0, depth = 0 }
