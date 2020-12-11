module Main where

import Data.Either
import Data.Maybe
import Data.Stack
import System.IO
import Text.Read
import qualified Data.Map.Strict as M

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
          | Help
          | MutateStack StackOperation

data WordTableEntry = WordTableEntry
  { word :: Main.Word
  , description :: String }

type WordTable = M.Map String WordTableEntry

interpreterWords :: WordTable
interpreterWords = M.fromList [("exit", WordTableEntry Exit "Quits the program")
                              ,("print", WordTableEntry Print "Print the stack")
                              ,("help", WordTableEntry Help "Print available words")

                              ,("drop", WordTableEntry (MutateStack Drop) "Remove top element of the stack")
                              ,("dup", WordTableEntry (MutateStack Duplicate) "Duplicate the top element on the stack")
                              ,("swap", WordTableEntry (MutateStack Swap) "Swap the order of the two most top elements on the stack")

                              ,("inc", WordTableEntry (MutateStack (UnaryOperation (safeUnaryOperation inc))) "Increase the value of the top element in the stack by 1")
                              ,("dec", WordTableEntry (MutateStack (UnaryOperation (safeUnaryOperation dec))) "Decrease the value of the top element in the stack by 1")
                              ,("sin", WordTableEntry (MutateStack (UnaryOperation (safeUnaryOperation sin))) "Sine function")
                              ,("cos", WordTableEntry (MutateStack (UnaryOperation (safeUnaryOperation cos))) "Cosine function")
                              ,("tan", WordTableEntry (MutateStack (UnaryOperation (safeUnaryOperation tan))) "Tangent function")
                              ,("sqrt", WordTableEntry (MutateStack (UnaryOperation safeSquareRoot)) "Square root")

                              ,("+", WordTableEntry (MutateStack (BinaryOperation (safeBinaryOperation (+)))) "Addition")
                              ,("-", WordTableEntry (MutateStack (BinaryOperation (safeBinaryOperation (-)))) "Subtraction")
                              ,("*", WordTableEntry (MutateStack (BinaryOperation (safeBinaryOperation (*)))) "Multiplication")
                              ,("^", WordTableEntry (MutateStack (BinaryOperation (safeBinaryOperation (**)))) "Raise to the power")
                              ,("/", WordTableEntry (MutateStack (BinaryOperation safeDivide)) "Divide")

                              ,("pi", WordTableEntry (MutateStack (Enter pi)) "π")]
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

parseWord :: String -> WordTable -> Maybe Main.Word
parseWord s table =
  case M.lookup s table of
    Just entry -> Just (word entry)
    _ -> do
      number <- readMaybe s
      Just (MutateStack (Enter number))

prettyPrintStack :: Show a => Stack a -> String
prettyPrintStack s = "size: " ++ show (stackSize s) ++ "\nitems:" ++ items s
  where
    items s = case stackPop s of
      Just (s1, item) -> items s1 ++ " " ++ show item
      _ -> ""

evalWord :: Stack Double -> String -> WordTable -> IO (Maybe (Stack Double))
evalWord s w t = do
  case parseWord w t of
    Just Print -> do
      putStrLn (prettyPrintStack s)
      return (Just s)
    Just Help -> do
      putStrLn helpString
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
  where
    helpString = M.foldlWithKey (\prev key value -> prev ++ "\n" ++ key ++ "\t" ++ description value) "Available words" t

evalWords :: Stack Double -> Stack String -> WordTable -> IO (Maybe (Stack Double))
evalWords s w t = case stackPop w of
  Just (restOfW, headOfW) -> do
     result <- evalWord s headOfW t
     case result of
       Just newS -> evalWords newS restOfW t
       Nothing -> return Nothing
  _ -> return (Just s)

-- TODO: Use side effects
repl :: Stack Double -> IO (Maybe (Stack Double))
repl s = do 
      putStr "(rpn calculator) "
      hFlush stdout
      userLine <- getLine
      let userWords = words userLine
      result <- evalWords s (listToStack userWords) t
      case result of
        Just newS -> repl newS
        Nothing -> return Nothing
  where
    listToStack l = listToStackInner stackNew (reverse l)
    listToStackInner s l = if null l
      then s
      else listToStackInner (stackPush s (head l)) (tail l)
    t = interpreterWords

-- TODO Exit codes
main :: IO ()
main = do
  hPutStrLn stderr "Welcome to the RPN calculator\nTo learn how to use PRN read https://en.wikipedia.org/wiki/Reverse_Polish_notation\nUse the \"help\" command to list available words."
  repl stackNew
  return ()
