module LibSpec where

import Test.Hspec

import Data.Either

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

  describe "parseTag" $ do
    it "parses an open tag" $ do
      parse' parseTag "<>"    `shouldBe` Right ""
      parse' parseTag "<a>"   `shouldBe` Right "a"
      parse' parseTag "<AAA>" `shouldBe` Right "AAA"
      parse' parseTag "<7>"   `shouldBe` Right "7"
      parse' parseTag "<8xU>" `shouldBe` Right "8xU"

    context "when given an invalid input" $
      it "fails" $ do
        parse' parseTag ""       `shouldSatisfy` isLeft
        parse' parseTag "<a"     `shouldSatisfy` isLeft
        parse' parseTag "A>"     `shouldSatisfy` isLeft
        parse' parseTag "</7>"   `shouldSatisfy` isLeft
        parse' parseTag "< 8xU>" `shouldSatisfy` isLeft
        parse' parseTag "< >"    `shouldSatisfy` isLeft
        parse' parseTag "< />"   `shouldSatisfy` isLeft
        parse' parseTag "<a >"   `shouldSatisfy` isLeft
        parse' parseTag "<!>"    `shouldSatisfy` isLeft
        parse' parseTag "<#a$>"  `shouldSatisfy` isLeft

  describe "parseCloseTag" $ do
    it "parses an close tag" $ do
      parse' parseCloseTag "</>"    `shouldBe` Right ""
      parse' parseCloseTag "</a>"   `shouldBe` Right "a"
      parse' parseCloseTag "</AAA>" `shouldBe` Right "AAA"
      parse' parseCloseTag "</7>"   `shouldBe` Right "7"
      parse' parseCloseTag "</8xU>" `shouldBe` Right "8xU"

      parse' parseCloseTag "</a >"  `shouldBe` Right "a"
      parse' parseCloseTag "</a  >" `shouldBe` Right "a"

    context "when given an invalid input" $
      it "fails" $ do
        parse' parseCloseTag ""        `shouldSatisfy` isLeft
        parse' parseCloseTag "</a"     `shouldSatisfy` isLeft
        parse' parseCloseTag "</a "    `shouldSatisfy` isLeft
        parse' parseCloseTag "/A>"     `shouldSatisfy` isLeft
        parse' parseCloseTag "<7>"     `shouldSatisfy` isLeft
        parse' parseCloseTag "</ 8xU>" `shouldSatisfy` isLeft
        parse' parseCloseTag "< >"     `shouldSatisfy` isLeft
        parse' parseCloseTag "< />"    `shouldSatisfy` isLeft
        parse' parseCloseTag "</!>"    `shouldSatisfy` isLeft
        parse' parseCloseTag "</#a$>"  `shouldSatisfy` isLeft
