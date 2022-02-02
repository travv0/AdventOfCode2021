#!/usr/bin/env cabal
{- cabal:
build-depends: base, shelly, text, directory, filepath
ghc-options: -Wall
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import           Data.Foldable                  ( for_ )
import           Data.List                      ( intercalate
                                                , sort
                                                )
import           Data.Maybe                     ( mapMaybe )
import qualified Data.Text                     as T
import qualified Shelly                        as S
import           System.Directory               ( doesFileExist )
import           System.Environment             ( getArgs )
import           System.FilePath                ( (<.>)
                                                , (</>)
                                                )
import           Text.Read                      ( readMaybe )

padNum :: Int -> Int -> String
padNum n i = let s = show i in replicate (n - length s) '0' <> s

main :: IO ()
main = do
    selectedNums <- mapMaybe (readMaybe :: String -> Maybe Int) <$> getArgs

    let nums = if null selectedNums
            then [1 .. 25]
            else sort . filter (\i -> i > 0 && i <= 25) $ selectedNums

    putStrLn
        $  "\nRunning "
        <> (case nums of
               _ | length nums == 25 -> "all days"
               [num] -> "day " <> show num
               _ -> "days " <> intercalate ", " (map show nums)
           )
        <> "\n"

    for_ nums $ \i -> do
        putStrLn $ "Day " <> show i <> ":"
        let dir        = "day" <> padNum 2 i
            file       = "day" <> show i
            path       = dir </> file
            fsharpPath = path <.> "fsx"

        fsharpPathExists  <- doesFileExist fsharpPath
        ocamlPathExists   <- doesFileExist $ path <.> "ml"
        haskellPathExists <- doesFileExist $ path <.> "cabal"

        S.shelly . S.verbosely $ do
            if
                | fsharpPathExists -> do
                    S.run_
                        "dotnet"
                        ["fsi", T.pack fsharpPath, T.pack (dir </> "input.txt")]
                | ocamlPathExists -> do
                    S.cd dir
                    S.which "esy.exe" >>= \case
                        Just esy -> do
                            S.silently $ S.run_ esy []
                            S.run_ esy ["run"]
                        Nothing -> do
                            S.silently $ S.run_ "powershell" ["esy.ps1"]
                            S.run_ "powershell" ["esy.ps1", "run"]
                | haskellPathExists -> do
                    S.cd dir
                    S.run_ "cabal" ["run", "-v0", T.pack $ "day" <> show i]
                | otherwise -> S.echo $ "Nothing to run for day " <> T.pack
                    (show i)

            S.echo ""
