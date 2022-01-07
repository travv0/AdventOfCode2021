module Part2 where

import           Data.Foldable                  ( foldl' )
import           Lib                            ( Command(..) )

data Coords = Coords
  { depth    :: Integer
  , position :: Integer
  , aim      :: Integer
  }

moveSub :: Coords -> (Command, Integer) -> Coords
moveSub cs (command, units) = case command of
  Forward ->
    cs { position = position cs + units, depth = depth cs + aim cs * units }
  Down -> cs { aim = aim cs + units }
  Up   -> cs { aim = aim cs - units }

coords :: [(Command, Integer)] -> Coords
coords = foldl' moveSub Coords { position = 0, depth = 0, aim = 0 }
