import Test.Hspec

import WordNumber
import Cipher
import StdFunctions

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



  describe "cipherChar" $ do
     it "encodes a character with an offset" $ do
        cipherChar 1 'a' `shouldBe` 'b'

     it "encodes a character with an offset with mod" $ do
        cipherChar 122 'a' `shouldBe` 'b'

  describe "cipher" $ do
      it "encodes a message with an offset" $ do
          cipher  1 "hello" `shouldBe` "ifmmp"

      it "encodes a whole sentence, correctly" $ do
          cipher 20 "hi, john! What is going on?" `shouldBe`
            "\ETX\EOT@4\ENQ\n\ETX\t54k\ETXu\SI4\EOT\SO4\STX\n\EOT\t\STX4\n\tS"

  describe "myOr" $ do
       it "myOr will work like or in prelude" $ do
           myOr [True, True, True, False] `shouldBe` True

  describe "myAny" $ do
       it "myAny will work like any in prelude" $ do
            myAny even [1,2,3] `shouldBe` True

  describe "myElem" $ do
       it "myElem works like elem in prelude" $ do
          myElem 'a' "aeiou" `shouldBe` True

  describe "myElemAny" $ do
      it "myElemAny works like myElem, but uses any" $ do
         myElemAny 'a' "aeiou" `shouldBe` True

  describe "cipher to unCipher" $ do
    it "unCipher applied to cipher with give back the same sentence" $ do
        unCipher 20 msg `shouldBe` "hello, John!"
           where msg = cipher 20 "hello, John!"
