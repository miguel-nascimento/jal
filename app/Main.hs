module Main where

import Syntax.Ast
import Syntax.Parser
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "No argument given"
    (fileName : _) -> do
      fileContent <- readFile fileName
      let ast = parseFile fileContent fileName
      case ast of
        Left err -> putStrLn err
        Right ast' -> print ast'