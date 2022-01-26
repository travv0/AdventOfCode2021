{-# LANGUAGE NamedFieldPuns #-}
module Lib
    ( parseInput
    , countLitPixels
    , enhanceTimes
    ) where

import           Data.Char                      ( digitToInt )
import           Data.Foldable                  ( foldl' )
import           Data.List.Split                ( splitOn )
import qualified Data.Map                      as Map
import           Data.Map                       ( Map )
import qualified Data.Vector                   as Vector
import           Data.Vector                    ( (!)
                                                , Vector
                                                )

data Pixel = Light | Dark deriving (Show, Eq)

type Coords = (Int, Int)

type EnhancementAlg = Vector Pixel

type Image = Map Coords Pixel

parseInput :: String -> (EnhancementAlg, Image)
parseInput input = case splitOn "\n\n" input of
    [alg, img] -> (algorithm, image)
      where
        algorithm = Vector.fromList $ map charToPixel alg
        image =
            Map.fromList
                . concat
                . zipWith
                      (\y row ->
                          ((-1, y), Dark) -- add a column to left of start image
                                          -- for detecting the default infinite
                                          -- pixel type
                              : zipWith
                                    (\x char -> ((x, y), charToPixel char))
                                    [0 ..]
                                    row
                      )
                      [0 ..]
                $ lines img
    _ -> error "bad parse"

charToPixel :: Char -> Pixel
charToPixel '#' = Light
charToPixel '.' = Dark
charToPixel p   = error $ "bad pixel " <> show p

getOutputPixel :: Pixel -> EnhancementAlg -> Image -> Coords -> Pixel
getOutputPixel def alg image coords =
    let index = base2ToInt $ getBinary def image coords in alg ! index

getBinary :: Pixel -> Image -> Coords -> String
getBinary def image (x, y) = map (pixelToBit . pixelOrDefault) coordsGrid
  where
    pixelToBit Dark  = '0'
    pixelToBit Light = '1'
    makeCoords dx dy = (x + dx, y + dy)
    coordsGrid     = flip makeCoords <$> [-1, 0, 1] <*> [-1, 0, 1]
    pixelOrDefault = flip (Map.findWithDefault def) image

base2ToInt :: String -> Int
base2ToInt = foldl' (\acc x -> acc * 2 + digitToInt x) 0

data Bounds = Bounds
    { minX :: Int
    , maxX :: Int
    , minY :: Int
    , maxY :: Int
    }
    deriving Show

enhanceImage :: EnhancementAlg -> Image -> Image
enhanceImage alg image =
    let bounds = getBounds image
    in
        Map.fromList
        . map
              (\c -> (c, getOutputPixel (defaultPixel bounds image) alg image c)
              )
        $ relevantCoords bounds

defaultPixel :: Bounds -> Image -> Pixel
defaultPixel Bounds { minX, minY } image = image Map.! (minX, minY)

relevantCoords :: Bounds -> [(Int, Int)]
relevantCoords Bounds { minX, maxX, minY, maxY } =
    flip (,) <$> [minY - 1 .. maxY + 1] <*> [minX - 1 .. maxX + 1]

getBounds :: Image -> Bounds
getBounds image =
    foldl'
            (\Bounds { minX, maxX, minY, maxY } (x, y) -> Bounds
                { minX = min minX x
                , maxX = max maxX x
                , minY = min minY y
                , maxY = max maxY y
                }
            )
            Bounds { minX = maxBound
                   , maxX = minBound
                   , minY = maxBound
                   , maxY = minBound
                   }
        $ Map.keys image

enhanceTimes :: Int -> EnhancementAlg -> Image -> Image
enhanceTimes 0 _   image = image
enhanceTimes n alg image = enhanceTimes (n - 1) alg $ enhanceImage alg image

countLitPixels :: Image -> Int
countLitPixels image = length . filter ((==) Light . snd) $ Map.toList image
