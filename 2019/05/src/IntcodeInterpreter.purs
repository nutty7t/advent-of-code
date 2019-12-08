module Main where

import Prelude

import Data.Array (catMaybes, fromFoldable, replicate, reverse, snoc, uncons, updateAt, updateAtIndices, zip, (!!), (..))
import Data.Either (Either(..))
import Data.Foldable (class Foldable, length)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split, trim)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), uncurry)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Text.Parsing.Parser (ParseError, Parser, fail, parseErrorMessage, runParser)
import Text.Parsing.Parser.Combinators (manyTill, option)
import Text.Parsing.Parser.String (eof)
import Text.Parsing.Parser.Token (digit)

type Address = Int

type Memory = Array Int

type Counter = Address

data Opcode = Add | Mult | Read | Write | JumpT | JumpF | Less | Eq | Halt

instance showOpcode :: Show Opcode where
  show Add   = "Add"
  show Mult  = "Multiply"
  show Read  = "Read"
  show Write = "Write"
  show JumpT = "Jump-If-True"
  show JumpF = "Jump-If-False"
  show Less  = "Less Than"
  show Eq    = "Equals"
  show Halt  = "Halt"

data Mode = Position | Immediate

instance showMode :: Show Mode where
  show Position  = "Position"
  show Immediate = "Immediate"

type Instruction =
  { opcode :: Opcode
  , params :: Array Mode
  }

type State =
  { pc     :: Counter
  , ram    :: Memory
  , input  :: Array Int
  , output :: Array Int
  }

type Error = String

-- | A parser for opcodes.
opcode :: Parser String Opcode
opcode = do
  d1 <- digit
  d2 <- option '0' digit
  case fromCharArray [d1, d2] of
    "10"      -> pure Add
    "20"      -> pure Mult
    "30"      -> pure Read
    "40"      -> pure Write
    "50"      -> pure JumpT
    "60"      -> pure JumpF
    "70"      -> pure Less
    "80"      -> pure Eq
    "99"      -> pure Halt
    otherwise -> fail "unknown opcode"

-- | A parser for parameter modes.
mode :: Parser String Mode
mode = do
  m <- digit
  case m of
    '0'       -> pure Position
    '1'       -> pure Immediate
    otherwise -> fail "unknown parameter mode"

-- | Appends missing parameter modes (due to leading zeros) to a sequence of
-- | parsed modes. Any missing modes are `0` (Position).
allModes :: forall f. Foldable f => Opcode -> f Mode -> Array Mode
allModes c xs = fromFoldable xs <> replicate (arity c - length xs) Position

updateWriteMode :: Opcode -> Array Mode -> Array Mode
updateWriteMode op modes =
  case op of
    Add       -> updateAtIndices [Tuple 2 Immediate] modes
    Mult      -> updateAtIndices [Tuple 2 Immediate] modes
    Read      -> updateAtIndices [Tuple 0 Immediate] modes
    Less      -> updateAtIndices [Tuple 2 Immediate] modes
    Eq        -> updateAtIndices [Tuple 2 Immediate] modes
    otherwise -> modes

-- | A parser for instructions.
instruction :: Parser String Instruction
instruction = do
  code  <- opcode
  modes <- manyTill mode eof
  pure
    { opcode: code
    , params: updateWriteMode code $ allModes code modes
    }

-- | Returns the number of arguments that an instruction takes.
arity :: Opcode -> Int
arity op =
  case op of
    Add   -> 3
    Mult  -> 3
    Read  -> 1
    Write -> 1
    JumpT -> 2
    JumpF -> 2
    Less  -> 3
    Eq    -> 3
    Halt  -> 0

-- | Parses the error message from a `ParseError`.
parseError :: forall a. Either ParseError a -> Either Error a
parseError (Left e)  = Left $ parseErrorMessage e
parseError (Right a) = Right a

-- | Reverses a string.
reverse' :: String -> String
reverse' = fromCharArray <<< reverse <<< toCharArray

-- | Parses an Intcode instruction.
parseInstruction :: String -> Either Error Instruction
parseInstruction s = parseError $ runParser (reverse' s) instruction

-- | Reads all of the arguments of the current Intcode instruction.
readArguments :: State -> Array Mode -> Either String (Array Int)
readArguments s xs = sequence $ uncurry (readArgument s) <$> zip xs (1 .. length xs)

-- | Reads the nth argument of the current Intcode instruction.
readArgument :: State -> Mode -> Int -> Either Error Int
readArgument s m n =
  case m of
    Position  -> dereference s.ram $ s.pc + n
    Immediate -> read        s.ram $ s.pc + n

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
interpret s = do
  v    <- read s.ram s.pc
  i    <- parseInstruction $ show v
  args <- readArguments s i.params
  case i.opcode of
    Add   -> executeAdd s args
    Mult  -> executeMult s args
    Read  -> executeRead s args
    Write -> executeWrite s args
    JumpT -> executeJumpT s args
    JumpF -> executeJumpF s args
    Less  -> executeLess s args
    Eq    -> executeEq s args
    Halt  -> pure s

-- | Execute add instruction.
executeAdd :: State -> Array Int -> Either Error State
executeAdd s args =
  case args of
    [a, b, target] -> do
      ram <- write s.ram target $ a + b
      pure s { ram = ram
             , pc  = s.pc + 4
             }
    otherwise ->
      Left "argument mismatch"

-- | Execute multiply instruction.
executeMult :: State -> Array Int -> Either Error State
executeMult s args =
  case args of
    [a, b, target] -> do
      ram <- write s.ram target $ a * b
      pure s { ram = ram
             , pc  = s.pc + 4
             }
    otherwise ->
      Left "argument mismatch"

-- | Execute read instruction.
executeRead :: State -> Array Int -> Either Error State
executeRead s args =
  case args of
    [target] -> do
      case uncons s.input of
        Just input -> do
          ram <- write s.ram target input.head
          pure s { ram = ram
                 , pc  = s.pc + 2
                 , input = input.tail
                 }
        Nothing ->
          Left "input buffer is empty"
    otherwise ->
      Left "argument mismatch"

-- | Execute write instruction.
executeWrite :: State -> Array Int -> Either Error State
executeWrite s args =
  case args of
    [value]   -> Right s { pc = s.pc + 2
                         , output = snoc s.output value
                         }
    otherwise -> Left "argument mismatch"

-- | Execute jump-if-true instruction.
executeJumpT :: State -> Array Int -> Either Error State
executeJumpT s args =
  case args of
    [v, a] | v /= 0 -> Right s { pc = a }
    [_, _]          -> Right s { pc = s.pc + 3 }
    otherwise       -> Left "argument mismatch"

-- | Execute jump-if-false instruction.
executeJumpF :: State -> Array Int -> Either Error State
executeJumpF s args =
  case args of
    [v, a] | v == 0 -> Right s { pc = a }
    [_, _]          -> Right s { pc = s.pc + 3 }
    otherwise       -> Left "argument mismatch"

-- | Execute less-than instruction.
executeLess :: State -> Array Int -> Either Error State
executeLess s args =
  case args of
    [a, b, target] -> do
       let result = if a < b then 1 else 0
       ram <- write s.ram target result
       pure s { ram = ram, pc  = s.pc + 4 }
    otherwise -> Left "argument mismatch"

-- | Execute equals instruction.
executeEq :: State -> Array Int -> Either Error State
executeEq s args =
  case args of
    [a, b, target] -> do
       let result = if a == b then 1 else 0
       ram <- write s.ram target result
       pure s { ram = ram, pc  = s.pc + 4 }
    otherwise -> Left "argument mismatch"

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

-- | Load the program into memory (Array Int) and set the program counter to 0.
-- | This is the initial state of the program.
parseProgram :: String -> Array Int -> State
parseProgram p i =
  let
    ram = catMaybes $ fromString <$> trim <$> split (Pattern ",") p
  in
    { ram: ram
    , pc: 0
    , input: i
    , output: []
    }

-- | Parse the output of the program.
parseOutput :: Either Error State -> String
parseOutput s =
  case s of
    Left e   -> "no output"
    Right s' -> show $ s'.output

main :: Effect Unit
main = do
  contents <- readTextFile UTF8 "./src/IntcodeProgram.txt"
  let initialStatePart1 = parseProgram contents [1]
  log $ "Part 1: " <> (parseOutput $ runProgram initialStatePart1)
  let initialStatePart2 = parseProgram contents [5]
  log $ "Part 2: " <> (parseOutput $ runProgram initialStatePart2)
