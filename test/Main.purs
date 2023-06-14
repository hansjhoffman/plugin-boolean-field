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
    it "should handle 't' shorthand" do
      let
        actual = parse_ "t"
        expected = true
      (fromRight false actual) `shouldEqual` expected
    it "should handle 'y' shorthand" do
      let
        actual = parse_ "y"
        expected = true
      (fromRight false actual) `shouldEqual` expected
    it "should handle '1' shorthand" do
      let
        actual = parse_ "1"
        expected = true
      (fromRight false actual) `shouldEqual` expected
    it "should handle 'f' shorthand" do
      let
        actual = parse_ "f"
        expected = false
      (fromRight true actual) `shouldEqual` expected
    it "should handle 'n' shorthand" do
      let
        actual = parse_ "n"
        expected = false
      (fromRight true actual) `shouldEqual` expected
    it "should handle '0' shorthand" do
      let
        actual = parse_ "0"
        expected = false
      (fromRight true actual) `shouldEqual` expected
    it "should handle 'on'" do
      let
        actual = parse_ "on"
        expected = true
      (fromRight false actual) `shouldEqual` expected
    it "should handle 'true'" do
      let
        actual = parse_ "true"
        expected = true
      (fromRight false actual) `shouldEqual` expected
    it "should handle 'yes'" do
      let
        actual = parse_ "yes"
        expected = true
      (fromRight false actual) `shouldEqual` expected
    it "should handle 'off'" do
      let
        actual = parse_ "off"
        expected = false
      (fromRight false actual) `shouldEqual` expected
    it "should handle 'false'" do
      let
        actual = parse_ "false"
        expected = false
      (fromRight false actual) `shouldEqual` expected
    it "should handle 'no'" do
      let
        actual = parse_ "no"
        expected = false
      (fromRight false actual) `shouldEqual` expected
    it "should fail on 't' shorthand" do
      let
        actual = parse_ "tx"
        expected = errMsg
      (fromLeft dummyMsg actual) `shouldEqual` expected
    it "should fail on 'y' shorthand" do
      let
        actual = parse_ "yx"
        expected = errMsg
      (fromLeft dummyMsg actual) `shouldEqual` expected
    it "should fail on '1' shorthand" do
      let
        actual = parse_ "10"
        expected = errMsg
      (fromLeft dummyMsg actual) `shouldEqual` expected
    it "should fail on unknown shorthand" do
      let
        actual = parse_ "x"
        expected = errMsg
      (fromLeft dummyMsg actual) `shouldEqual` expected
    it "should fail on 'f' shorthand" do
      let
        actual = parse_ "foo"
        expected = errMsg
      (fromLeft dummyMsg actual) `shouldEqual` expected
    it "should fail on 'n' shorthand" do
      let
        actual = parse_ "nx"
        expected = errMsg
      (fromLeft dummyMsg actual) `shouldEqual` expected
    it "should fail on '0' shorthand" do
      let
        actual = parse_ "01"
        expected = errMsg
      (fromLeft dummyMsg actual) `shouldEqual` expected
