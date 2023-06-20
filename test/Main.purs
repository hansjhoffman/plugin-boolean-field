module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Main (parse_)
import Test.Spec (describe, it)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.Utils (assertLeft, assertRight)

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  describe "parse_" do
    let
      errMsg = "Expected one of [ 't', 'f', 'y', 'n', '1', '0', 'on', 'off', 'yes', 'no', 'true', 'false' ]"
    it "should handle all valid shorthand values" do
      assertRight (parse_ "t") true
      assertRight (parse_ "y") true
      assertRight (parse_ "1") true
      assertRight (parse_ "f") false
      assertRight (parse_ "n") false
      assertRight (parse_ "0") false
    it "should handle all valid longhand values" do
      assertRight (parse_ "true") true
      assertRight (parse_ "yes") true
      assertRight (parse_ "on") true
      assertRight (parse_ "false") false
      assertRight (parse_ "no") false
      assertRight (parse_ "off") false
    it "should fail on 'close' values" do
      assertLeft (parse_ "tr") errMsg
      assertLeft (parse_ "ye") errMsg
      assertLeft (parse_ "10") errMsg
      assertLeft (parse_ "01") errMsg
      assertLeft (parse_ "yess") errMsg
