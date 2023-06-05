module Test.Main where

import Prelude

import Data.Either (fromLeft, fromRight)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Main (parse)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "parse" do
    it "should handle 't' shorthand" do
      let
        actual = parse "t"
        expected = true
      (fromRight false actual) `shouldEqual` expected
    it "should handle 'y' shorthand" do
      let
        actual = parse "y"
        expected = true
      (fromRight false actual) `shouldEqual` expected
    it "should handle '1' shorthand" do
      let
        actual = parse "1"
        expected = true
      (fromRight false actual) `shouldEqual` expected
    it "should handle 'f' shorthand" do
      let
        actual = parse "f"
        expected = false
      (fromRight true actual) `shouldEqual` expected
    it "should handle 'n' shorthand" do
      let
        actual = parse "n"
        expected = false
      (fromRight true actual) `shouldEqual` expected
    it "should handle '0' shorthand" do
      let
        actual = parse "0"
        expected = false
      (fromRight true actual) `shouldEqual` expected
    it "should handle 'on'" do
      let
        actual = parse "on"
        expected = true
      (fromRight false actual) `shouldEqual` expected
    it "should handle 'true'" do
      let
        actual = parse "true"
        expected = true
      (fromRight false actual) `shouldEqual` expected
    it "should handle 'yes'" do
      let
        actual = parse "yes"
        expected = true
      (fromRight false actual) `shouldEqual` expected
    it "should handle 'off'" do
      let
        actual = parse "off"
        expected = false
      (fromRight false actual) `shouldEqual` expected
    it "should handle 'false'" do
      let
        actual = parse "false"
        expected = false
      (fromRight false actual) `shouldEqual` expected
    it "should handle 'no'" do
      let
        actual = parse "no"
        expected = false
      (fromRight false actual) `shouldEqual` expected
    it "should fail on 't' shorthand" do
      let
        actual = parse "tx"
        expected = "Expected end of string"
      (fromLeft "uh-oh" actual) `shouldEqual` expected
    it "should fail on 'y' shorthand" do
      let
        actual = parse "yx"
        expected = "Expected end of string"
      (fromLeft "uh-oh" actual) `shouldEqual` expected
    it "should fail on '1' shorthand" do
      let
        actual = parse "10"
        expected = "Expected end of string"
      (fromLeft "uh-oh" actual) `shouldEqual` expected
    it "should fail on unknown shorthand" do
      let
        actual = parse "x"
        expected = "Expected one of ['f','n','0']"
      (fromLeft "uh-oh" actual) `shouldEqual` expected
    it "should fail on 'f' shorthand" do
      let
        actual = parse "foo"
        expected = "Expected end of string"
      (fromLeft "uh-oh" actual) `shouldEqual` expected
    it "should fail on 'n' shorthand" do
      let
        actual = parse "nx"
        expected = "Expected end of string"
      (fromLeft "uh-oh" actual) `shouldEqual` expected
    it "should fail on '0' shorthand" do
      let
        actual = parse "01"
        expected = "Expected end of string"
      (fromLeft "uh-oh" actual) `shouldEqual` expected
