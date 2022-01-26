module Main where

import           Lib                            ( countLitPixels
                                                , enhanceTimes
                                                , parseInput
                                                )

main :: IO ()
main = do
  (alg, image) <- parseInput <$> readFile "input.txt"
  let printLitPixelCountAfter n =
        putStrLn
          $  "After enhancing the original input image "
          <> show n
          <> " times, "
          <> (show . countLitPixels $ enhanceTimes n alg image)
          <> " pixels are lit"

  printLitPixelCountAfter 2
  printLitPixelCountAfter 50
