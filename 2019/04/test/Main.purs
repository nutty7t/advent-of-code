module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Main (adjacent, digits, monotonic, strictDouble, validate)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "Password Validator" do
    it "validates Part 1 passwords" do
      let validatePart1 = validate [monotonic, adjacent 2]
      (validatePart1 $ digits 111111) `shouldEqual` true
      (validatePart1 $ digits 223450) `shouldEqual` false
      (validatePart1 $ digits 123789) `shouldEqual` false

    it "validates Part 2 passwords" do
      let validatePart2 = validate [monotonic, strictDouble]
      (validatePart2 $ digits 112233) `shouldEqual` true
      (validatePart2 $ digits 123444) `shouldEqual` false
      (validatePart2 $ digits 111122) `shouldEqual` true
