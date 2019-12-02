module Main where

import Prelude

import Data.Array (catMaybes)
import Data.Foldable (sum)
import Data.Int (fromString)
import Data.String (Pattern(..), split, trim)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

type Fuel = Int
type Mass = Int

-- | Returns an array of the parsable module masses in the string.
parseModules :: String -> Array Mass
parseModules contents = catMaybes $ fromString <$> trim <$> split (Pattern "\n") contents

-- | Fuel required to launch a given module is based on its mass. Specifically,
-- | to find the fuel required for a module, take its mass, divide by three,
-- | round down, and subtract 2.
computeModuleFuel :: Mass -> Fuel
computeModuleFuel mass = mass / 3 - 2

-- | Compute the fuel requirements for each module and then sum the fuel
-- | requirements for all modules to yield the total fuel requirement.
computeFuelRequirements :: Array Mass -> Fuel
computeFuelRequirements = sum <<< map computeModuleFuel

-- | Uh oh, it turns out that fuel itself requires fuel.
computeModuleFuel' :: Mass -> Fuel
computeModuleFuel' mass = go 0 mass
  where
    cmf = computeModuleFuel
    go acc m | cmf m <= 0 = acc
    go acc m | otherwise  = go (acc + cmf m) (cmf m)

-- | Let's recalculate the total fuel requirements taking into account the
-- | fuel's fuel, and the fuel's fuel's fuel, and the fuel's fuel's [...] fuel.
computeFuelRequirements' :: Array Mass -> Fuel
computeFuelRequirements' = sum <<< map computeModuleFuel'

main :: Effect Unit
main = do
  contents <- readTextFile UTF8 "./src/SpacecraftModules.txt"
  let modules = parseModules contents
  log $ "Part 1: " <> (show $ computeFuelRequirements modules)
  log $ "Part 2: " <> (show $ computeFuelRequirements' modules)
