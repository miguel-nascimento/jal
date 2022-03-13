module Syntax.Parser where

import Data.Functor (($>))
import Data.Text as T
import Data.Void
import Syntax.Ast
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

pSpace :: Parser ()
pSpace =
  L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")

pIdentifier :: Parser String
pIdentifier = some letterChar

pString :: Parser String
pString =
  between
    (char '"')
    (char '"')
    (many $ noneOf "\"")

pInteger :: Parser Integer
pInteger = L.signed (return ()) L.decimal

pBool :: Parser Bool
pBool = pTrue <|> pFalse
  where
    pTrue = string "true" $> True
    pFalse = string "false" $> False

pLiteral :: Parser Literal
pLiteral =
  choice
    [ pString >>= \s -> pure $ LStr (T.pack s),
      pInteger >>= \i -> pure $ LInt i,
      pBool >>= \b -> pure $ LBool b
    ]