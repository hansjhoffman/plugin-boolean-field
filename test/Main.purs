module Test.Main where

import Prelude

import Data.Either (fromLeft, fromRight)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Main (parse_)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "parse_" do
    let
      errMsg = "Expected one of [ 't', 'f', 'y', 'n', '1', '0', 'on', 'off', 'yes', 'no', 'true', 'false' ]"
      dummyMsg = "foobar"
    it "should handle all valid shorthand values" do
      (fromRight false $ parse_ "t") `shouldEqual` true
      (fromRight false $ parse_ "y") `shouldEqual` true
      (fromRight false $ parse_ "1") `shouldEqual` true
      (fromRight true $ parse_ "f") `shouldEqual` false
      (fromRight true $ parse_ "n") `shouldEqual` false
      (fromRight true $ parse_ "0") `shouldEqual` false
    it "should handle all valid longhand values" do
      (fromRight false $ parse_ "true") `shouldEqual` true
      (fromRight false $ parse_ "yes") `shouldEqual` true
      (fromRight false $ parse_ "on") `shouldEqual` true
      (fromRight true $ parse_ "false") `shouldEqual` false
      (fromRight true $ parse_ "no") `shouldEqual` false
      (fromRight true $ parse_ "off") `shouldEqual` false
    it "should fail on 'close' values" do
      (fromLeft dummyMsg $ parse_ "tr") `shouldEqual` errMsg
      (fromLeft dummyMsg $ parse_ "ye") `shouldEqual` errMsg
      (fromLeft dummyMsg $ parse_ "10") `shouldEqual` errMsg
      (fromLeft dummyMsg $ parse_ "01") `shouldEqual` errMsg
      (fromLeft dummyMsg $ parse_ "yess") `shouldEqual` errMsg
