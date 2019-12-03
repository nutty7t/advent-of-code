module Main where

import Prelude

import Control.MonadPlus (guard)
import Data.Array (catMaybes, head, updateAt, (..), (!!))
import Data.Either (Either(..))
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.String (trim)
import Data.String.Common (split)
import Data.String.Pattern (Pattern(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

type Address = Int
type Counter = Address
type Memory = Array Int
type State = Tuple Counter Memory
type Instruction = Int -> Int -> Int
type Error = String

-- | Load the program into memory (Array Int) and set the program counter to 0.
-- | This is the initial state of the program.
parseProgram :: String -> State
parseProgram p = Tuple 0 $ catMaybes $ fromString <$> trim <$> split (Pattern ",") p

-- | Restore the gravity assist program to the "1202 program alarm" state it
-- | had just before the last computer caught fire by writing initial input
-- | values to memory.
restoreGravityAssist :: State -> Int -> Int -> Either Error State
restoreGravityAssist (Tuple c m) noun verb = do
  m'  <- write m 1 noun
  m'' <- write m' 2 verb
  pure $ Tuple c m''

-- | Read the value of a memory location.
read :: Memory -> Address -> Either Error Int
read m a =
  case m !! a of
    Nothing -> Left "segmentation fault"
    Just x  -> Right x

-- | Dereference a pointer.
dereference :: Memory -> Address -> Either Error Int
dereference m = read m >=> read m

-- | Write a value to a memory location.
write :: Memory -> Address -> Int -> Either Error Memory
write m a v =
  case updateAt a v m of
    Nothing -> Left "segmentation fault"
    Just m' -> Right m'

-- | Execute the opcode instruction pointed to by program counter.
interpret :: State -> Either Error State
interpret s@(Tuple c m) = do
  opcode <- read m c
  case opcode of
    1         -> execute s (+)
    2         -> execute s (*)
    99        -> pure s
    otherwise -> Left "encountered unknown opcode"

-- | Execute a binary operation instruction.
execute :: State -> Instruction -> Either Error State
execute (Tuple c m) op = do
  arg1   <- dereference m $ c + 1
  arg2   <- dereference m $ c + 2
  target <- read m $ c + 3
  let result = op arg1 arg2
  let next = c + 4
  Tuple <$> pure next <*> write m target result

-- | Compute a fixed-point of a function given an initial state. An error may
-- | occur before we converge to a fixed-point; in that case, return the error.
fixpoint :: forall a. Eq a => (a -> Either Error a) -> Either Error a -> Either Error a
fixpoint f (Left x)  = Left x
fixpoint f (Right x) =
  case f x of
    Left e  -> Left e
    Right s -> if s == x
                 then Right x
                 else fixpoint f $ f s

-- | Run the program by repeatedly executing instructions until a fixed-point
-- | is reached (i.e. Opcode #99). Converging to a fixed-point indicates the
-- | successful completion of a program.
runProgram :: State -> Either Error State
runProgram s = fixpoint interpret $ pure s

-- | Parse the output (available at address 0) of the program.
parseOutput :: Either Error State -> Maybe Int
parseOutput s =
  case s of
    Left e            -> Nothing
    Right (Tuple c m) -> head m

-- | Format the Part 1 output for printing to the console.
formatOutput :: Maybe Int -> String
formatOutput o =
  case o of
    Nothing     -> "result not available"
    Just result -> show result

-- | Find a pair of inputs that produces a desired output.
findInputs :: State -> Int -> Array (Array Int)
findInputs s o = do
  noun <- 0 .. 99
  verb <- 0 .. 99
  let output = restoreGravityAssist s noun verb >>= runProgram
  guard $ parseOutput output == Just o
  pure $ [noun, verb]

-- | Format the Part 2 output for printing to the console.
formatOutput' :: Array (Array Int) -> String
formatOutput' solutions =
  case head solutions of
    Just [noun, verb] -> show $ 100 * noun + verb
    otherwise         -> "no solutions"

main :: Effect Unit
main = do
  contents <- readTextFile UTF8 "./src/IntcodeProgram.txt"
  let initialState = parseProgram contents
  case restoreGravityAssist initialState 12 2 of
    Left e  -> log "Part 1: failed to restore gravity assist program state"
    Right s -> log $ "Part 1: " <> (formatOutput $ parseOutput $ runProgram s)
  log $ "Part 2: " <> (formatOutput' $ findInputs initialState 19690720)
