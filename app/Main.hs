{-|
    rpn_calculator is a reverse polish notation calculator.

    Copyright (C) 2021  Lior Stern

    This file is part of rpn_calculator.

    rpn_calculator is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    rpn_calculator is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with rpn_calculator.  If not, see <https://www.gnu.org/licenses/>.
-}

module Main where

import Data.Stack
import System.IO
import Interpreter


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
