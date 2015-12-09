import Test.Hspec

import WordNumber

main :: IO ()
main = hspec $ do
  describe "myWords" $ do
    it "single word without prepended space" $ do
      myWords prependEq prependNEq "all" `shouldBe` ["all"]

    it "single word with prepended space" $ do
        myWords prependEq prependNEq " all" `shouldBe` ["all"]

    it "two word sentence" $ do
            myWords prependEq prependNEq "  all it" `shouldBe` ["all", "it"]

    it "two word sentence" $ do
        myWords prependEq prependNEq "all it" `shouldBe` ["all", "it"]
