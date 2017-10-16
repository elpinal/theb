module LibSpec where

import Test.Hspec

import Lib

spec :: Spec
spec =
  describe "parseHTML" $
    it "parses HTML" $ do
      parse' parseHTML "<!DOCTYPE html><html>aaa</html>" `shouldBe` Right (Node "html" [Text "aaa"])
