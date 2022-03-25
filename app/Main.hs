module Main where

import qualified Data.Text as T
import Syntax.Ast
import Syntax.Parser
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "No argument given"
    (fileName : _) -> do
      parseFromFile fileName
      pure ()