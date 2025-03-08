module Test.Main where

import Prelude

import Effect (Effect)
import Test.Spec(Spec, describe, it, pending, parallel)
import Test.Spec.Assertions (shouldEqual, shouldNotEqual, shouldSatisfy, fail, AnyShow(..))
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (delay)

import Test.Spec.Reporter.June.Pretty (prettyReporter)

main :: Effect Unit
main = runSpecAndExitProcess [prettyReporter] do
  describe ":3" do
    it "works" do
      1 `shouldEqual` 1
      fail "it doesn't work :("
    pending "working at all what???"
  describe "nesting" do
    describe "nesting more" do
      describe "too deep aaa" do
        it "uhh" do
          "this" `shouldNotEqual` "that"
  parallel $ describe "parallel" do
    it "1" do 1 `shouldEqual` 1
    it "2" do 1 `shouldEqual` 1
    it "3" do 1 `shouldEqual` 1
    it "sloooow" do delay $ Milliseconds 1000.0
    it "4" do 1 `shouldEqual` 1
    it "5" do 1 `shouldEqual` 1
    describe "A" do
      it "super slow" do delay $ Milliseconds 5000.0
    describe "B" do
      it "fastish" do delay $ Milliseconds 50.0
