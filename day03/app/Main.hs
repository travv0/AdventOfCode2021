module Main where

import           Lib                            ( co2ScrubberRating
                                                , epsilonRate
                                                , gammaRate
                                                , oxygenGeneratorRating
                                                , parseCounts
                                                )

main :: IO ()
main = do
    input <- lines <$> readFile "input.txt"
    let counts = parseCounts input

    putStrLn $ "The power consumption of the submarine is " <> show
        (gammaRate counts * epsilonRate counts)

    putStrLn $ "The life support rating of the submarine is " <> show
        (oxygenGeneratorRating input * co2ScrubberRating input)
