module LibSpec where

import Test.Hspec

import Lib

spec :: Spec
spec =
  describe "parseHTML" $
    it "parses HTML" $ do
      parse' parseHTML "<!DOCTYPE html><html>aaa</html>" `shouldBe` Right (Node "html" [Text "aaa"])
      parse' parseHTML "<!DOCTYPE html><html><head></head><body></body></html>" `shouldBe` Right (Node "html" [Node "head" [], Node "body" []])
