module LibSpec where

import Test.Hspec

import Data.Either

import Lib

spec :: Spec
spec = do
  describe "parseHTML" $
    it "parses HTML" $ do
      parse' parseHTML "<!DOCTYPE html><html>aaa</html>" `shouldBe` Right (Node "html" [] [Text "aaa"])
      parse' parseHTML "<!DOCTYPE html><html><head></head><body></body></html>" `shouldBe` Right (Node "html" [] [Node "head" [] [], Node "body" [] []])

  describe "parseElement" $ do
    it "parses an element" $ do
      parse' parseElement "<html>aaa</html>" `shouldBe` Right (Node "html" [] [Text "aaa"])
      parse' parseElement "<html><head></head><body></body></html>" `shouldBe` Right (Node "html" [] [Node "head" [] [], Node "body" [] []])
      parse' parseElement "<html><head><title>aaa</title></head><body></body></html>" `shouldBe` Right (Node "html" [] [Node "head" [] [Node "title" [] [Text "aaa"]], Node "body" [] []])

    it "parses a void element" $ do
      parse' parseElement "<meta>" `shouldBe` Right (Node "meta" [] [])

    context "when given an invalid input" $
      it "fails" $ do
        parse' parseElement "<aaa>" `shouldSatisfy` isLeft

  describe "parseStartTag" $ do
    it "parses a start tag" $ do
      parse' parseStartTag "<>"    `shouldBe` Right ("", [])
      parse' parseStartTag "<a>"   `shouldBe` Right ("a", [])
      parse' parseStartTag "<AAA>" `shouldBe` Right ("AAA", [])
      parse' parseStartTag "<7>"   `shouldBe` Right ("7", [])
      parse' parseStartTag "<8xU>" `shouldBe` Right ("8xU", [])

    context "when given an invalid input" $
      it "fails" $ do
        parse' parseStartTag ""       `shouldSatisfy` isLeft
        parse' parseStartTag "<a"     `shouldSatisfy` isLeft
        parse' parseStartTag "A>"     `shouldSatisfy` isLeft
        parse' parseStartTag "</7>"   `shouldSatisfy` isLeft
        parse' parseStartTag "< 8xU>" `shouldSatisfy` isLeft
        parse' parseStartTag "< >"    `shouldSatisfy` isLeft
        parse' parseStartTag "< />"   `shouldSatisfy` isLeft
        parse' parseStartTag "<a >"   `shouldSatisfy` isLeft
        parse' parseStartTag "<!>"    `shouldSatisfy` isLeft
        parse' parseStartTag "<#a$>"  `shouldSatisfy` isLeft

  describe "parseEndTag" $ do
    it "parses an end tag" $ do
      parse' parseEndTag "</>"    `shouldBe` Right ""
      parse' parseEndTag "</a>"   `shouldBe` Right "a"
      parse' parseEndTag "</AAA>" `shouldBe` Right "AAA"
      parse' parseEndTag "</7>"   `shouldBe` Right "7"
      parse' parseEndTag "</8xU>" `shouldBe` Right "8xU"

      parse' parseEndTag "</a >"  `shouldBe` Right "a"
      parse' parseEndTag "</a  >" `shouldBe` Right "a"

    context "when given an invalid input" $
      it "fails" $ do
        parse' parseEndTag ""        `shouldSatisfy` isLeft
        parse' parseEndTag "</a"     `shouldSatisfy` isLeft
        parse' parseEndTag "</a "    `shouldSatisfy` isLeft
        parse' parseEndTag "/A>"     `shouldSatisfy` isLeft
        parse' parseEndTag "<7>"     `shouldSatisfy` isLeft
        parse' parseEndTag "</ 8xU>" `shouldSatisfy` isLeft
        parse' parseEndTag "< >"     `shouldSatisfy` isLeft
        parse' parseEndTag "< />"    `shouldSatisfy` isLeft
        parse' parseEndTag "</!>"    `shouldSatisfy` isLeft
        parse' parseEndTag "</#a$>"  `shouldSatisfy` isLeft

  describe "parseAttr" $ do
    it "parses an attribute" $ do
      parse' parseAttr "a"       `shouldBe` Right ("a", "")
      parse' parseAttr "a=b"     `shouldBe` Right ("a", "b")
      parse' parseAttr "a='b'"   `shouldBe` Right ("a", "b")
      parse' parseAttr "a=\"b\"" `shouldBe` Right ("a", "b")
      parse' parseAttr "a8Z=343" `shouldBe` Right ("a8Z", "343")

    it "parses an attribute where an equal sign is surrounded by any whitespace" $ do
      parse' parseAttr "a = b"     `shouldBe` Right ("a", "b")
      parse' parseAttr "a = 'b'"   `shouldBe` Right ("a", "b")
      parse' parseAttr "a = \"b\"" `shouldBe` Right ("a", "b")
      parse' parseAttr "a8Z = 343" `shouldBe` Right ("a8Z", "343")

    context "when the value is quoted by single-/double-quotes" $
      it "parses an attribute whose value contains some non-alphanumeric characters" $ do
        parse' parseAttr "a=' '"              `shouldBe` Right ("a", " ")
        parse' parseAttr "a=\" \""            `shouldBe` Right ("a", " ")
        parse' parseAttr "a='abc\"-#@$<aa> '" `shouldBe` Right ("a", "abc\"-#@$<aa> ")

    context "when given an invalid input" $
      it "fails" $ do
        parse' parseAttr "a="   `shouldSatisfy` isLeft
        parse' parseAttr "a='"  `shouldSatisfy` isLeft
        parse' parseAttr "a=\"" `shouldSatisfy` isLeft
