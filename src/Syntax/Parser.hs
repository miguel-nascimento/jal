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
    (L.skipLineComment "--")
    (L.skipBlockComment "{--" "--}")

symbol :: String -> Parser String
symbol = L.symbol pSpace

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

pIdentifier :: Parser Identifier
pIdentifier = label "identifier" $ some (letterChar <|> char '_')

pString :: Parser String
pString =
  label "string" $
    between
      (char '"')
      (char '"')
      (many $ noneOf "\"")

pInteger :: Parser Integer
pInteger = label "integer" $ L.signed (pure ()) L.decimal

pBool :: Parser Bool
pBool = label "bool" $ pTrue <|> pFalse
  where
    pTrue = string "true" $> True
    pFalse = string "false" $> False

pVar :: Parser Expr
pVar = Var <$> pIdentifier

pFunction :: Parser Expr
pFunction =
  label "function" $
    string "let" *> do
      pSpace
      name <- pIdentifier
      pSpace
      args <- parens (pIdentifier `sepBy` pSpace) <|> pure []
      pSpace
      body <- string "=" *> pSpace *> pExpr
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
  label "lambda" $
    string "\\" *> pSpace *> do
      args <- pIdentifier
      body <- pSpace *> string "->" *> pSpace *> pExpr
      pure $ Lamb args body

pOpApp :: Parser Expr
pOpApp = do
  expr1 <- pExprWithoutOp
  pSpace
  op <- pOp
  pSpace
  expr2 <- pExprWithoutOp
  pure $ OpApp op expr1 expr2
  where
    pExprWithoutOp = choice [pLamb, pVar, pFunction, Lit <$> pLiteral]
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

-- TODO: handle later tehe
-- pApp :: Parser Expr
-- pApp = normalParsing <|> parens normalParsing
--   where
--     normalParsing = do
--       expr1 <- try pExpr
--       pSpace
--       expr2 <- pExpr
--       pure $ App expr1 expr2

pExpr :: Parser Expr
pExpr =
  choice
    [ pFunction,
      pLamb,
      try pOpApp,
      pVar,
      Lit <$> pLiteral
    ]

parseFile :: String -> String -> Either String [Expr]
parseFile input filename =
  let outputE =
        parse
          (between pSpace eof (pExpr `sepBy` pSpace)) -- TODO: still need to handle eof orz
          filename
          input
   in case outputE of
        Left err -> Left $ errorBundlePretty err
        Right output -> Right output