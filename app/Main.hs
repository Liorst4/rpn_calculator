module Main where

import Data.Either
import Data.Maybe
import Data.Stack
import System.IO
import Text.Read

data StackOperation = Enter Double
                    | Drop
                    | Duplicate
                    | Swap
                    | UnaryOperation (Double -> Maybe Double)
                    | BinaryOperation (Double -> Double -> Maybe Double)

data StackOperationError = Underflow
                         | Undefined
                         deriving (Show)

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
    UnaryOperation op -> case stackPop s of
      Just (newStack1, x) -> case op x of
        Just newX -> Left (stackPush newStack1 newX)
        _ -> Right Undefined
      _ -> Right Underflow
    BinaryOperation op -> case stackPop s of
      Just (newStack1, y) -> case stackPop newStack1 of
        Just (newStack2, x) -> case op x y of
          Just newX -> Left (stackPush newStack2 newX)
          _ -> Right Undefined
        _ -> Right Underflow
      _ -> Right Underflow
  where
    unpackResult result error = maybe (Right error) Left result

data Word = Exit
          | Print
          | MutateStack StackOperation

parseWord :: String -> Maybe Main.Word
parseWord s =
  case s of
    "exit" -> Just Exit
    "print" -> Just Print
    "drop" -> Just (MutateStack Drop)
    "dup" -> Just (MutateStack Duplicate)
    "swap" -> Just (MutateStack Swap)

    "inc" -> Just (MutateStack (UnaryOperation (safeUnaryOperation inc)))
    "dec" -> Just (MutateStack (UnaryOperation (safeUnaryOperation dec)))
    "sin" -> Just (MutateStack (UnaryOperation (safeUnaryOperation sin)))
    "cos" -> Just (MutateStack (UnaryOperation (safeUnaryOperation cos)))
    "tan" -> Just (MutateStack (UnaryOperation (safeUnaryOperation tan)))
    "sqrt" -> Just (MutateStack (UnaryOperation safeSquareRoot))

    "+" -> Just (MutateStack (BinaryOperation (safeBinaryOperation (+))))
    "-" -> Just (MutateStack (BinaryOperation (safeBinaryOperation (-))))
    "*" -> Just (MutateStack (BinaryOperation (safeBinaryOperation (*))))
    "^" -> Just (MutateStack (BinaryOperation (safeBinaryOperation (**))))
    "/" -> Just (MutateStack (BinaryOperation safeDivide))

    "pi" -> Just (MutateStack (Enter pi))
    _ -> do
      number <- readMaybe s
      Just (MutateStack (Enter number))
  where
    safeDivide x y = if y /= 0
                     then Just (x / y)
                     else Nothing
    safeSquareRoot x = if x >= 0
                       then Just (sqrt x)
                       else Nothing
    safeUnaryOperation op x = Just (op x)
    safeBinaryOperation op x y = Just (op x y)
    inc = (+) 1
    dec = (-) 1

evalWord :: Stack Double -> String -> IO (Maybe (Stack Double))
evalWord s w = do
  case parseWord w of
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

evalWords :: Stack Double -> Stack String -> IO (Maybe (Stack Double))
evalWords s w = case stackPop w of
  Just (restOfW, headOfW) -> do
     result <- evalWord s headOfW
     case result of
       Just newS -> evalWords newS restOfW
       Nothing -> return Nothing
  _ -> return (Just s)

-- TODO: Print prompt
-- TODO: Use side effects
repl :: Stack Double -> IO (Maybe (Stack Double))
repl s = do 
      userLine <- getLine
      let userWords = words userLine
      result <- evalWords s (listToStack userWords)
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
  hPutStrLn stderr "Welcome to the RPN calculator\nTo learn how to use PRN read https://en.wikipedia.org/wiki/Reverse_Polish_notation"
  let numberStack = stackNew
  repl numberStack
  return ()
