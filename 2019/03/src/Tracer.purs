module Main where

import Prelude

import Control.Alt ((<|>))
import Data.Array (catMaybes, concat, head, last, singleton, snoc, sort, take)
import Data.Either (Either(..))
import Data.Foldable (foldMap, foldl)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromJust)
import Data.Ord (abs)
import Data.String (Pattern(..), split)
import Data.String.CodeUnits as S
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), uncurry)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Partial.Unsafe (unsafePartial)
import Text.Parsing.Parser (Parser, parseErrorMessage, runParser)
import Text.Parsing.Parser.Combinators (many1Till)
import Text.Parsing.Parser.String (eof, string)
import Text.Parsing.Parser.Token (digit)

-- | Represents the directional orientation of a wire segment. Cardinal
-- | direction names are used because Left conflicts with the Left data
-- | constructor of the Either type.
data Direction = North | East | South | West

instance showDirection :: Show Direction where
  show d =
    case d of
      North -> "Up"
      South -> "Down"
      West  -> "Left"
      East  -> "Right"

-- | Represents a position on the grid relative to the central port.
type Coordinates =
  { x :: Int
  , y :: Int
  }

-- | Represents the magnitude and the direction of a wire segment.
type UntracedSegment =
  { direction :: Direction
  , length    :: Int
  }

-- | Represents a wire, which is a concatenated list of untraced segments.
-- | The start and end coordinates of each segment is unknown.
type UntracedWire = Array UntracedSegment

-- | Represents the start and end coordinates of an untraced segment along with
-- | the position of the start coordinate on the wire.
type TracedSegment =
  { start  :: Coordinates
  , end    :: Coordinates
  , vector :: UntracedSegment
  , pos    :: Int
  }

-- | Also represents a wire, but the coordinates of each segment and their
-- | positions on the wire are known.
type TracedWire = Array TracedSegment

-- | Represents an intersection point between two wires, and the sum of the
-- | wire steps from the central port to the intersection point.
type Intersection =
  { point :: Coordinates
  , steps :: Int
  , s1    :: TracedSegment
  , s2    :: TracedSegment
  }

-- | Represents an error.
type Error = String

-- | Segment direction parser combinator.
direction :: Parser String Direction
direction = (string "U" >>= \_ -> pure North)
            <|> (string "D" >>= \_ -> pure South)
            <|> (string "L" >>= \_ -> pure West)
            <|> (string "R" >>= \_ -> pure East)

-- | Segment magnitude parser combinator.
length :: Parser String Int
length = unsafePartial fromJust
  <$> fromString
  <$> foldMap S.singleton
  <$> many1Till digit eof

-- | Segment parser combinator.
segment :: Parser String UntracedSegment
segment = do
  d <- direction
  l <- length
  pure { direction: d
       , length: l
       }

-- | Parses a segment.
parseSegment :: String -> Either Error UntracedSegment
parseSegment s =
  case runParser s segment of
    Left e   -> Left $ parseErrorMessage e
    Right s' -> Right s'

-- | Parses a wire.
parseWire :: String -> Either Error UntracedWire
parseWire s = sequence $ parseSegment <$> split (Pattern ",") s

-- | Parses the input file contents.
parseWires :: String -> Either Error (Tuple UntracedWire UntracedWire)
parseWires s =
  case take 2 $ split (Pattern "\n") s of
    [a, b]    -> Tuple <$> parseWire a <*> parseWire b
    otherwise -> Left "Invalid or corrupt file contents"

-- | Finds the end point of an untraced segment given the starting point.
findEnd :: UntracedSegment -> Coordinates -> Coordinates
findEnd s c =
  case s.direction of
    North -> { x: c.x, y: c.y + s.length }
    South -> { x: c.x, y: c.y - s.length }
    West  -> { x: c.x - s.length, y: c.y }
    East  -> { x: c.x + s.length, y: c.y }

-- | Traces the next segment of a wire.
traceSegment :: TracedWire -> UntracedSegment -> TracedWire
traceSegment w s =
  case last w of
    Just last -> snoc w
      $ { start: last.end
        , end: findEnd s last.end
        , vector: s
        , pos: last.pos + last.vector.length
        }
    Nothing -> singleton
      $ { start: start
        , end: findEnd s start
        , vector: s
        , pos: 0
        }
  where
    start = { x: 0, y: 0 }

-- | Traces a wire.
traceWire :: UntracedWire -> TracedWire
traceWire = foldl traceSegment []

-- | Computes the intersection point of two segments.
-- | http://en.wikipedia.org/wiki/Line-line_intersection
intersect :: TracedSegment -> TracedSegment -> Maybe Intersection
intersect s1 s2 =
  let
    -- Line s1: a1 * x + b1 * y = c1
    a1 = s1.end.y - s1.start.y
    b1 = s1.start.x - s1.end.x
    c1 = a1 * s1.start.x + b1 * s1.start.y

    -- Line s2: a2 * x + b2 * y = c2
    a2 = s2.end.y - s2.start.y
    b2 = s2.start.x - s2.end.x
    c2 = a2 * s2.start.x + b2 * s2.start.y

    -- Compute the intersection point.
    determinant = a1 * b2 - a2 * b1
    x = (b2 * c1 - b1 * c2) / determinant
    y = (a1 * c2 - a2 * c1) / determinant
    intersection = { x: x, y: y }

    -- Does the intersection point lay on both of the line segments?
    pointOnSegment :: Coordinates -> TracedSegment -> Boolean
    pointOnSegment c { start: s, end: e } =
      min s.x e.x <= c.x && c.x <= max s.x e.x
      && min s.y e.y <= c.y && c.y <= max s.y e.y

    existsOnSegments = pointOnSegment intersection s1
                       && pointOnSegment intersection s2

    -- Compute the sum of the wire steps.
    steps = abs (intersection.x - s1.start.x)
      + abs (intersection.x - s2.start.x)
      + abs (intersection.y - s1.start.y)
      + abs (intersection.y - s2.start.y)
      + s1.pos
      + s2.pos
  in
    case determinant of
      0         -> Nothing -- parallel
      otherwise -> if existsOnSegments && intersection /= { x: 0, y: 0 }
                     then Just { point: intersection
                               , steps: steps
                               , s1: s1
                               , s2: s2
                               }
                     else Nothing

-- | Finds all of the points where two wires intersect.
findIntersections :: TracedWire -> TracedWire -> Array Intersection
findIntersections a b =
  catMaybes $ uncurry intersect <$> concat do
    a' <- a
    b' <- b
    pure [Tuple a' b']

-- | Returns the intersection with the shortest Manhattan distance path from
-- | the central port.
closestDistance :: Array Intersection -> Maybe Int
closestDistance intersections = head $ sort $ manhattan <$> intersections
  where
    manhattan :: Intersection -> Int
    manhattan i = abs i.point.x + abs i.point.y

-- | Returns the intersection with the shortest step distance path from the
-- | central port.
closestDistance' :: Array Intersection -> Maybe Int
closestDistance' intersections = head $ sort $ (\{ steps } -> steps) <$> intersections

main :: Effect Unit
main = do
  contents <- readTextFile UTF8 "./src/Wires.txt"
  case parseWires contents of
    Left e            -> log e
    Right (Tuple a b) -> case findIntersections (traceWire a) (traceWire b) of
                           [] -> do
                             log "Part 1: no solutions"
                             log "Part 2: no solutions"
                           i -> do
                             log $ "Part 1: " <> (show $ closestDistance i)
                             log $ "Part 2: " <> (show $ closestDistance' i)
