module Test.Main where

import Prelude

import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Main (parseProgram, restoreGravityAssist, runProgram)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "Intcode Parser" do
    it "parses programs" do
      let input = "1,2,3,0,99"
      let output = parseProgram input
      output `shouldEqual` Tuple 0 [1, 2, 3, 0, 99]

  describe "Gravity Assist Program Restore" do
    it "sets input values" do
      let input = Tuple 0 [1, 0, 0, 0, 99]
      let output = restoreGravityAssist input 7 8
      output `shouldEqual` Right (Tuple 0 [1, 7, 8, 0, 99])

  describe "Intcode Interpreter" do
    it "interprets programs" do
      let input = Tuple 0 [1, 1, 1, 4, 99, 5, 6, 0, 99]
      let output = runProgram input
      output `shouldEqual` Right (Tuple 8 [30, 1, 1, 4, 2, 5, 6, 0, 99])
