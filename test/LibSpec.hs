module LibSpec where

import Test.Hspec

import Lib

spec :: Spec
spec = do
  describe "parseHTML" $
    it "parses HTML" $ do
      parse' parseHTML "<!DOCTYPE html><html>aaa</html>" `shouldBe` Right (Node "html" [Text "aaa"])
      parse' parseHTML "<!DOCTYPE html><html><head></head><body></body></html>" `shouldBe` Right (Node "html" [Node "head" [], Node "body" []])

  describe "parseElement" $
    it "parses an element" $ do
      parse' parseElement "<html>aaa</html>" `shouldBe` Right (Node "html" [Text "aaa"])
      parse' parseElement "<html><head></head><body></body></html>" `shouldBe` Right (Node "html" [Node "head" [], Node "body" []])
      parse' parseElement "<html><head><title>aaa</title></head><body></body></html>" `shouldBe` Right (Node "html" [Node "head" [Node "title" [Text "aaa"]], Node "body" []])
