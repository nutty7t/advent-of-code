module Main where

import Prelude

import Data.Array (catMaybes, fromFoldable)
import Data.Foldable (elem, foldMap, foldr, sum)
import Data.List (List(..), difference, length, singleton, (:))
import Data.Map (Map, empty, insert, keys, lookup, unionWith)
import Data.Maybe (Maybe(..))
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (unwrap)
import Data.String (Pattern(..), split, trim)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

type Object = String
type OrbitMap = Map String (List String)
type Error = String

-- | Computes the transitive closure of the orbit map.
closure :: Object -> OrbitMap -> Int
closure o m = sum $ go m <$> (fromFoldable $ keys m)
  where
    go :: OrbitMap -> Object -> Int
    go m' o' =
      case lookup o' m' of
        Just xs -> (length xs)
                     + (unwrap
                         $ foldMap (\e -> Additive $ go m e) xs)
        Nothing -> 0

-- | Parses an orbit as a directed edge.
parseEdge :: Array String -> Maybe OrbitMap
parseEdge edge =
  case edge of
    [tail, head] ->
      Just $ insert tail (singleton head) empty
    otherwise ->
      Nothing

-- | Parses the orbit map as an undirected edge.
parseMap :: String -> OrbitMap
parseMap s = foldr (unionWith (<>)) empty edges
  where
    edges :: Array OrbitMap
    edges = catMaybes
      $ parseEdge
        <$> split (Pattern ")")
        <$> trim
        <$> split (Pattern "\n") s

-- | Parses an orbit as an undirected edge.
parseEdge' :: Array String -> Maybe OrbitMap
parseEdge' edge =
  case edge of
    [tail, head] ->
      Just $ insert head (singleton tail)
               (insert tail (singleton head) empty)
    otherwise ->
      Nothing

-- | Parses the orbit map as a directed edge.
parseMap' :: String -> OrbitMap
parseMap' s = foldr (unionWith (<>)) empty edges
  where
    edges :: Array OrbitMap
    edges = catMaybes
      $ parseEdge'
        <$> split (Pattern ")")
        <$> trim
        <$> split (Pattern "\n") s

-- | Finds the length of the shortest path to Santa from a starting point.
toSanta :: Object -> OrbitMap -> Int
toSanta o m = go m o Nil 0
  where
    go :: OrbitMap -> Object -> List Object -> Int -> Int
    go m' o' v d =
      case lookup o' m' of
        -- ignore self loops and already visited nodes
        Just xs -> case difference xs v of
          Nil -> 0
          xs' -> if elem "SAN" xs
            then d - 1
            else unwrap
                 $ foldMap (\e -> Additive
                                  $ go m e (e : v) (d + 1)) xs'
        Nothing -> 0

main :: Effect Unit
main = do
  contents <- readTextFile UTF8 "./src/OrbitMap.txt"
  log $ "Part 1: " <> (show $ closure "COM" $ parseMap contents)
  log $ "Part 2: " <> (show $ toSanta "YOU" $ parseMap' contents)
