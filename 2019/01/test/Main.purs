module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Main (Mass, computeModuleFuel, computeModuleFuel', parseModules)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "Spaceship Module Parser" do
    it "returns empty array when no lines are parsable" do
      let input = ""
      let output = parseModules input
      output `shouldEqual` ([] :: Array Mass)
    it "ignores bad lines" do
      let input = """
      1
      2
      three
      4
      """
      let output = parseModules input
      output `shouldEqual` [1, 2, 4]

  describe "Part 1: computeModuleFuel" do
    it "computes module fuel requirements" do
      computeModuleFuel 12     `shouldEqual` 2
      computeModuleFuel 14     `shouldEqual` 2
      computeModuleFuel 1969   `shouldEqual` 654
      computeModuleFuel 100756 `shouldEqual` 33583

  describe "Part 2: computeModuleFuel" do
    it "computes module fuel requirements" do
       computeModuleFuel' 14     `shouldEqual` 2
       computeModuleFuel' 1969   `shouldEqual` 966
       computeModuleFuel' 100756 `shouldEqual` 50346
