module Unit.Data.SGF.Parse.EncodingsSpec where

import Data.SGF.Parse.Encodings

import Test.Hspec


main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "guessEncoding" $ do
    it "does not hang" $
      guessEncoding [85,84,70,45,56] `shouldBe` [encodingFromString "utf-8"]
