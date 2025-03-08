module Test.Main where

import Prelude

import Effect (Effect)
import Test.Spec(Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy, fail, AnyShow(..))
import Test.Spec.Runner.Node (runSpecAndExitProcess)

import Test.Spec.Reporter.June.Pretty (prettyReporter)

main :: Effect Unit
main = runSpecAndExitProcess [prettyReporter] do
  describe ":3" do
    it "works" do
      1 `shouldEqual` 1
      fail "it doesn't work :("
