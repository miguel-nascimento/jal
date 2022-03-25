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
      fileContent <- readFile fileName
      let ast = parseFile fileName (T.pack fileContent)
      case ast of
        Left err -> putStrLn (T.unpack err)
        Right ast' -> print ast'