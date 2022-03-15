module Syntax.Parser where

import Data.Functor (($>))
import qualified Data.Text as T
import Data.Void
import Syntax.Ast
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

pSpace :: Parser ()
pSpace =
  L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")

symbol :: String -> Parser String
symbol = L.symbol pSpace

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

pIdentifier :: Parser Identifier
pIdentifier = some (letterChar <|> char '_')

pString :: Parser String
pString =
  between
    (char '"')
    (char '"')
    (many $ noneOf "\"")

pInteger :: Parser Integer
pInteger = L.signed (pure ()) L.decimal

pBool :: Parser Bool
pBool = pTrue <|> pFalse
  where
    pTrue = string "true" $> True
    pFalse = string "false" $> False

pVar :: Parser Expr
pVar = Var <$> pIdentifier

pFunction :: Parser Expr
pFunction =
  string "let" *> pSpace *> do
    name <- pIdentifier <* pSpace
    args <- some pIdentifier <|> pure [] -- TODO: why `let fun x y = ..` is not working?
    body <- pSpace *> symbol "=" *> pSpace *> pExpr
    pure $ Fn name args body

pLiteral :: Parser Literal
pLiteral =
  choice
    [ pString >>= \s -> pure $ LStr (T.pack s),
      pInteger >>= \i -> pure $ LInt i,
      pBool >>= \b -> pure $ LBool b
    ]

pLamb :: Parser Expr
pLamb =
  string "\\" *> pSpace *> do
    args <- pIdentifier
    body <- pSpace *> string "->" *> pSpace *> pExpr
    pure $ Lamb args body

pOpApp :: Parser Expr
pOpApp = do
  expr1 <- pLamb <|> pVar <|> pFunction <|> Lit <$> pLiteral
  pSpace
  op <- pOp
  pSpace
  expr2 <- pLamb <|> pVar <|> pFunction <|> Lit <$> pLiteral
  pure $ OpApp op expr1 expr2
  where
    pOp =
      choice
        [ string "+" $> Add,
          string "-" $> Sub,
          string "*" $> Mul,
          string "/" $> Div,
          string "==" $> Eq,
          string "!=" $> Neq,
          string "<" $> Lt,
          string ">" $> Gt,
          string "<=" $> Le,
          string ">=" $> Ge
        ]

pApp :: Parser Expr
pApp = do
  expr1 <- pLamb <|> pVar <|> pFunction <|> Lit <$> pLiteral
  pSpace
  expr2 <- pLamb <|> pVar <|> pFunction <|> Lit <$> pLiteral
  pure $ App expr1 expr2

pExpr :: Parser Expr
pExpr =
  pSpace
    *> choice
      [ pFunction,
        pLamb,
        pApp,
        try pOpApp,
        pVar,
        Lit <$> pLiteral
      ]