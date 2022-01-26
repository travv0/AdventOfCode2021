import           GHC.Exts                       ( fromList )
import           Lib
import           Test.Hspec                     ( hspec
                                                , it
                                                , shouldBe
                                                )

main :: IO ()
main = hspec $ do
  it "parses input" $ do
    parseInput "3,4,3,1,2" `shouldBe` fromList [0, 1, 1, 2, 1, 0, 0, 0, 0]

  it "steps" $ do
    step (fromList [0, 1, 1, 2, 1, 0, 0, 0, 0])
      `shouldBe` fromList [1, 1, 2, 1, 0, 0, 0, 0, 0]
    step (fromList [1, 1, 2, 1, 0, 0, 0, 0, 0])
      `shouldBe` fromList [1, 2, 1, 0, 0, 0, 1, 0, 1]
    step (fromList [1, 2, 1, 0, 0, 0, 1, 0, 1])
      `shouldBe` fromList [2, 1, 0, 0, 0, 1, 1, 1, 1]

  it "produces the correct result after a number of steps" $ do
    sum (stepTimes 18 (fromList [0, 1, 1, 2, 1, 0, 0, 0, 0])) `shouldBe` 26
    sum (stepTimes 80 (fromList [0, 1, 1, 2, 1, 0, 0, 0, 0])) `shouldBe` 5934
    sum (stepTimes 256 (fromList [0, 1, 1, 2, 1, 0, 0, 0, 0]))
      `shouldBe` 26984457539
