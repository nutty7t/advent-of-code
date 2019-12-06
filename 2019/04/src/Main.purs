module Main where

import Prelude

import Data.Foldable (foldMap)
import Data.List (List(..), any, concat, difference, filter, length, nub, sort, take, transpose, (..), (:))
import Data.Monoid.Conj (Conj(..))
import Data.Newtype (unwrap)
import Data.Ord (abs)
import Effect (Effect)
import Effect.Console (log)

-- | Returns all final segments of the argument, longest first.
tails :: forall a. List a -> List (List a)
tails l = go Nil l
  where
    go acc Nil        = acc
    go acc t@(_ : xs) = t : go acc xs

-- | Returns a sliding window of a list of length `n`.
windows :: forall a. Int -> List a -> List (List a)
windows n xs = filter (eq n <<< length) $ transpose $ take n $ tails xs

-- | Returns true if all of the elements in the list are the same.
allSame :: forall a. Eq a => List a -> Boolean
allSame = eq 1 <<< length <<< nub

-- | Returns true if there exists a contiguous sequence of duplicate integers
-- | of length `n` in the list.
adjacent :: forall a. Eq a => Int -> List a -> Boolean
adjacent n xs = any allSame $ windows n xs

-- | Returns a list of integers that appear in the list as a contiguous
-- | sequence of length `n`.
consecutive :: forall a. Eq a => Int -> List a -> List a
consecutive n xs = nub <<< concat <$> filter allSame $ windows n xs

-- | Returns true iff there exists a contiguous sequence of duplicate elements
-- | of length 2 and that sequence is not a part of a longer sequence of
-- | duplicate elements.
strictDouble :: forall a. Eq a => List a -> Boolean
strictDouble xs = difference (consecutive 2 xs) (consecutive 3 xs) /= Nil

-- | Returns true iff the list of integers monotonically increases.
monotonic :: List Int -> Boolean
monotonic xs = xs == sort xs

-- | Validates a password using a list of validation functions.
validate :: Array (List Int -> Boolean) -> List Int -> Boolean
validate fns xs = unwrap <<< foldMap (\fn -> Conj $ fn xs) $ fns

-- | Converts an integer into an array of digits. Negative numbers will have
-- | their sign omitted from the result.
digits :: Int -> List Int
digits n = go Nil $ abs n
  where
    go acc i
      | i < 10    = i : acc
      | otherwise = go (i `mod` 10 : acc) (i / 10)

main :: Effect Unit
main = do
  let range = 193651 .. 649729
  let part1 = [monotonic, adjacent 2]
  let part2 = [monotonic, strictDouble]
  log $ "Part 1: " <> (show $ length $ filter (validate part1) (digits <$> range))
  log $ "Part 2: " <> (show $ length $ filter (validate part2) (digits <$> range))
