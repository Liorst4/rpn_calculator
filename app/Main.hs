module Main where

import Data.Either
import Data.Maybe
import Data.Stack
import System.IO
import Text.Read

data UnaryOperation = Increase
                    | Decrease
                    | SquareRoot
                    | Sine
                    | Cosine
                    | Tangent

unaryOperation :: UnaryOperation -> Double -> Maybe Double
unaryOperation safeOperation = case safeOperation of
                                 Increase -> justWrapper (1 +)
                                 Decrease -> justWrapper (1 -)
                                 SquareRoot -> safeSquareRoot
                                 Sine -> justWrapper sin
                                 Cosine -> justWrapper cos
                                 Tangent -> justWrapper tan
  where
    justWrapper = fmap Just
    safeSquareRoot x = if x >= 0
                       then Just (sqrt x)
                       else Nothing

data BinaryOperation = Add
                     | Subtract
                     | Multiply
                     | Divide
                     | RaiseToThePower
                     -- TODO LOG


binaryOperation :: BinaryOperation -> Double -> Double -> Maybe Double
binaryOperation safeOperation = case safeOperation of
                                  Add -> justWrapper (+)
                                  Subtract -> justWrapper (-)
                                  Multiply -> justWrapper (*)
                                  Divide -> safeDivide
                                  RaiseToThePower -> justWrapper (**)
  where
    justWrapper f x y = Just (f x y)
    safeDivide x y = if y /= 0
                     then Just (x / y)
                     else Nothing

data MathOperation = Unary UnaryOperation
                   | Binary BinaryOperation

data StackOperationError = Underflow
                         | Undefined
                         deriving (Show)

-- TODO Use side effects
performMathOperation :: MathOperation -> Stack Double -> Either (Stack Double) StackOperationError
performMathOperation operation s =
  case operation of
    Unary op -> case stackPop s of
      Just (newStack, x) -> case unaryOperation op x of
        Just newX -> Left (stackPush newStack newX)
        _ -> Right Undefined
      _ -> Right Underflow
    Binary op -> case stackPop s of
      Just (newStack1, y) -> case stackPop newStack1 of
        Just (newStack2, x) -> case binaryOperation op x y of
          Just newX -> Left (stackPush newStack2 newX)
          _ -> Right Undefined
        _ -> Right Underflow
      _ -> Right Underflow

data StackOperation = Enter Double
                    | Drop
                    | Duplicate
                    | Swap
                    | Calculate MathOperation

-- TODO Use side effect                   
performStackOperation :: StackOperation -> Stack Double -> Either (Stack Double) StackOperationError
performStackOperation operation s =
  case operation of
    Enter number -> Left (stackPush s number)
    Drop -> do
      let result = do
            (newStack, _) <- stackPop s
            Just newStack
      unpackResult result Underflow
    Duplicate -> do
      let result = do
            (_, topValue) <- stackPop s
            Just (stackPush s topValue)
      unpackResult result Underflow
    Swap -> do
      let result = do
            (newStack1, x) <- stackPop s
            (newStack2, y) <- stackPop newStack1
            let newStack3 = stackPush newStack2 x
            let newStack4 = stackPush newStack3 y
            Just newStack4
      unpackResult result Underflow
    Calculate mathOperation -> performMathOperation mathOperation s
  where
    unpackResult result error = maybe (Right error) Left result

data UserCommand = Exit
                 | Print
                 | MutateStack StackOperation

parseUserCommand :: String -> Maybe UserCommand
parseUserCommand s =
  case s of
    "exit" -> Just Exit
    "print" -> Just Print
    "drop" -> Just (MutateStack Drop)
    "dup" -> Just (MutateStack Duplicate)
    "swap" -> Just (MutateStack Swap)
    "inc" -> Just (MutateStack (Calculate (Unary Increase)))
    "dec" -> Just (MutateStack (Calculate (Unary Decrease)))
    "sqrt" -> Just (MutateStack (Calculate (Unary SquareRoot)))
    "sin" -> Just (MutateStack (Calculate (Unary Sine)))
    "cos" -> Just (MutateStack (Calculate (Unary Cosine)))
    "tan" -> Just (MutateStack (Calculate (Unary Tangent)))
    "+" -> Just (MutateStack (Calculate (Binary Add)))
    "-" -> Just (MutateStack (Calculate (Binary Subtract)))
    "*" -> Just (MutateStack (Calculate (Binary Multiply)))
    "/" -> Just (MutateStack (Calculate (Binary Divide)))
    "^" -> Just (MutateStack (Calculate (Binary RaiseToThePower)))
    "pi" -> Just (MutateStack (Enter pi))
    _ -> do
      number <- readMaybe s
      Just (MutateStack (Enter number))


executeWord :: Stack Double -> String -> IO (Maybe (Stack Double))
executeWord s w = do
  let userCommand = parseUserCommand w
  case userCommand of
    Just Print -> do
      print s -- TODO: Better print
      return (Just s)
    Just (MutateStack op) -> case performStackOperation op s of
      Left newStack -> return (Just newStack)
      Right error -> do
        hPutStrLn stderr ("Error: " ++ show error)
        return (Just s)
    Just Exit -> return Nothing
    _ -> do
      hPutStrLn stderr ("Invalid command: " ++ w)
      return (Just s)

executeWords :: Stack Double -> Stack String -> IO (Maybe (Stack Double))
executeWords s w = case stackPop w of
  Just (restOfW, headOfW) -> do
     result <- executeWord s headOfW
     case result of
       Just newS -> executeWords newS restOfW
       Nothing -> return Nothing
  _ -> return (Just s)

-- TODO: Print prompt
-- TODO: Use side effects
repl :: Stack Double -> IO (Maybe (Stack Double))
repl s = do 
      userLine <- getLine
      let userWords = words userLine
      result <- executeWords s (listToStack userWords)
      case result of
        Just newS -> repl newS
        Nothing -> return Nothing
  where
    listToStack l = listToStackInner stackNew (reverse l)
    listToStackInner s l = if null l
      then s
      else listToStackInner (stackPush s (head l)) (tail l)

-- TODO Exit codes
main :: IO ()
main = do
  let numberStack = stackNew
  repl numberStack
  return ()
