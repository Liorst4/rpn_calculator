module Main where

import Data.Maybe
import Data.Stack
import Text.Read

data UnaryOperation = Increase
                    | Decrease
                    -- TODO SQRT
                    -- TODO COS
                    -- TODO SIN
                    -- TODO TAN

unaryOperation :: UnaryOperation -> Double -> Double
unaryOperation operation = case operation of
                                   Increase -> (+) 1
                                   Decrease -> (-) 1

data BinaryOperation = Add
                     | Subtract
                     | Multiply
                     | Divide
                     -- TODO POW
                     -- TODO LOG


binaryOperation :: BinaryOperation -> Double -> Double -> Double 
binaryOperation operation = case operation of
                                    Add -> (+)
                                    Subtract -> (-)
                                    Multiply -> (*)
                                    Divide -> (/) -- TODO: Handle 0

data MathOperation = Unary UnaryOperation
                   | Binary BinaryOperation

-- TODO Use side effects
performMathOperation :: MathOperation -> Stack Double -> Maybe (Stack Double)
performMathOperation operation s =
  case operation of
    Unary op -> do
      (newStack, x) <- stackPop s
      let newX = unaryOperation op x
      Just (stackPush newStack newX)
    Binary op -> do
      (newStack1, y) <- stackPop s
      (newStack2, x) <- stackPop newStack1
      let newX = binaryOperation op x y
      Just (stackPush newStack2 newX)

data StackOperation = Enter Double
                    | Drop
                    | Calculate MathOperation
                    -- TODO SWAP
                    -- TODO DUP

-- TODO Use side effect                   
performStackOperation :: StackOperation -> Stack Double -> Maybe (Stack Double)
performStackOperation operation s =
  case operation of
    Enter number -> Just (stackPush s number)
    Drop -> do
      (newStack, _) <- stackPop s
      Just newStack
    Calculate mathOperation -> performMathOperation mathOperation s

data UserCommand = Exit
                 | Print
                 | MutateStack StackOperation

parseUserCommand :: String -> Maybe UserCommand
parseUserCommand s =
  case s of
    "exit" -> Just Exit
    "print" -> Just Print
    "drop" -> Just (MutateStack Drop)
    "inc" -> Just (MutateStack (Calculate (Unary Increase)))
    "dec" -> Just (MutateStack (Calculate (Unary Decrease)))
    "+" -> Just (MutateStack (Calculate (Binary Add)))
    "-" -> Just (MutateStack (Calculate (Binary Subtract)))
    "*" -> Just (MutateStack (Calculate (Binary Multiply)))
    "/" -> Just (MutateStack (Calculate (Binary Divide)))
    _ -> do
      number <- readMaybe s
      Just (MutateStack (Enter number))

-- TODO: Print prompt
-- TODO: Use side effects
repl :: Stack Double -> IO (Maybe (Stack Double))
repl s = do 
      userInput <- getLine
      -- TODO Seperate into words
      let userCommand = parseUserCommand userInput
      case userCommand of
        Just Print -> do
          print s -- TODO: Better print
          repl s
        Just (MutateStack op) -> do
          let newStack = performStackOperation op s
          repl (fromMaybe s newStack)
        Just Exit -> return Nothing
        _ -> repl s

-- TODO Exit codes
main :: IO ()
main = do
  let numberStack = stackNew
  repl numberStack
  return ()
