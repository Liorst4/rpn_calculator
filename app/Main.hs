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

definedDouble :: Double -> Maybe Double
definedDouble x = if isNaN x || isInfinite x then Nothing else Just x

-- TODO Use side effect                   
performStackOperation :: StackOperation -> Stack Double -> Either StackOperationError (Stack Double)
performStackOperation operation s =
  case operation of
    Enter number -> return $ stackPush s number
    Drop -> stackOrUnderflow $ do
      (newStack, _) <- stackPop s
      return newStack
    Duplicate -> stackOrUnderflow $ do
      (_, topValue) <- stackPop s
      return (stackPush s topValue)
    Swap -> stackOrUnderflow $ do
      (newStack1, x) <- stackPop s
      (newStack2, y) <- stackPop newStack1
      let newStack3 = stackPush newStack2 x
      let newStack4 = stackPush newStack3 y
      return newStack4
    UnaryOperation op -> case stackPop s of
      Just (newStack1, x) -> case op x >>= definedDouble of
        Just newX -> Right (stackPush newStack1 newX)
        _ -> Left Undefined
      _ -> Left Underflow
    BinaryOperation op -> case stackPop s of
      Just (newStack1, y) -> case stackPop newStack1 of
        Just (newStack2, x) -> case op x y >>= definedDouble of
          Just newX -> Right (stackPush newStack2 newX)
          _ -> Left Undefined
        _ -> Left Underflow
      _ -> Left Underflow
  where
    stackOrUnderflow maybeStack = case maybeStack of
      Just stack -> Right stack
      _ -> Left Underflow

data Word = Exit
          | Help
          | MutateStack StackOperation

data WordTableEntry = WordTableEntry
  { word :: Main.Word
  , description :: String }

type WordTable = M.Map String WordTableEntry

interpreterWords :: WordTable
interpreterWords = M.fromList [("exit", WordTableEntry Exit "Quits the program")
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
                              ,("log", WordTableEntry (MutateStack (BinaryOperation (safeBinaryOperation logBase))) "log function")

                              ,("e", WordTableEntry (MutateStack (Enter 2.718281828459045)) "e")
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
    Just entry -> return $ word entry
    _ -> do
      number <- readMaybe s
      safeNumber <- definedDouble number
      return $ MutateStack $ Enter safeNumber

prettyPrintStack :: Show a => Stack a -> String
prettyPrintStack s = "size: " ++ show (stackSize s) ++ "\nitems: " ++ items s
  where
    items s = case stackPop s of
      Just (s1, item) -> show item ++ " " ++ items s1
      _ -> ""

data EvalResult = UpdatedStack (Stack Double)
                | StackOperationError StackOperationError
                | ExitInterpreter
                | InvalidWord String

evalWord :: Stack Double -> String -> WordTable -> IO EvalResult
evalWord s w t = do
  case parseWord w t of
    Just Help -> do
      putStrLn helpString
      return (UpdatedStack s)
    Just (MutateStack op) -> case performStackOperation op s of
      Right newStack -> return (UpdatedStack newStack)
      Left error -> return (StackOperationError error)
    Just Exit -> return ExitInterpreter
    _ -> return (InvalidWord w)
  where
    helpString = M.foldlWithKey (\prev key value -> prev ++ "\n" ++ key ++ "\t" ++ description value) "Available words" t

evalWords :: Stack Double -> Stack String -> WordTable -> IO EvalResult
evalWords s w t = case stackPop w of
  Just (restOfW, headOfW) -> do
     result <- evalWord s headOfW t
     case result of
       UpdatedStack newS -> evalWords newS restOfW t
       _ -> return result
  _ -> return (UpdatedStack s)

-- TODO: Use side effects
repl :: Stack Double -> IO ()
repl s = do 
      putStr "(rpn calculator) "
      hFlush stdout
      userLine <- getLine -- TODO: Gnu readline
      let userWords = words userLine
      result <- evalWords s (listToStack userWords) t
      case result of
        UpdatedStack newS -> do
          putStrLn (prettyPrintStack newS)
          repl newS
        InvalidWord w -> do
          hPutStrLn stderr ("Invalid word: " ++ w)
          repl s
        StackOperationError e -> do
          hPutStrLn stderr ("Error: " ++ show e)
          repl s
        ExitInterpreter -> return ()
  where
    listToStack l = listToStackInner stackNew (reverse l)
    listToStackInner s l = if null l
      then s
      else listToStackInner (stackPush s (head l)) (tail l)
    t = interpreterWords

-- TODO Exit codes
main :: IO ()
main = do
  hPutStrLn stderr "Welcome to the RPN calculator\nTo learn how to use RPN read https://en.wikipedia.org/wiki/Reverse_Polish_notation\nUse the \"help\" command to list available words."
  repl stackNew
  return ()
