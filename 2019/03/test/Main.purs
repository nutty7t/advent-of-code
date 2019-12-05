module Test.Main where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Main (Intersection, closestDistance, closestDistance', findIntersections, parseWires, traceWire)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

runner :: String -> (Array Intersection -> Maybe Int) -> Maybe Int
runner input fn =
  case parseWires input of
    Left e            -> Nothing
    Right (Tuple a b) -> case findIntersections (traceWire a) (traceWire b) of
                           [] -> Nothing
                           i  -> fn i

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "Wire Tracer" do
    it "find shortest Manhattan distance intersection" do
      let input = "R75,D30,R83,U83,L12,D49,R71,U7,L72\n\
                  \U62,R66,U55,R34,D71,R55,D58,R83"
      let output = runner input closestDistance
      output `shouldEqual` Just 159

    it "find shortest step distance intersection" do
      let input = "R75,D30,R83,U83,L12,D49,R71,U7,L72\n\
                  \U62,R66,U55,R34,D71,R55,D58,R83"
      let output = runner input closestDistance'
      output `shouldEqual` Just 610
