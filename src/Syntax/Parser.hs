{-# LANGUAGE OverloadedStrings #-}

module Syntax.Parser where

import Data.Functor (($>), (<&>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Syntax.Ast
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 (L.skipLineComment "--") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

semicolon :: Parser Text
semicolon = label "semicolon" $ symbol ";"

pIdentifier :: Parser Text
pIdentifier =
  lexeme $
    label "identifier" $ do
      first <- letterChar
      rest <- many (alphaNumChar <|> char '_')
      pure $ T.pack (first : rest)

pString :: Parser Text
pString =
  label "string" $ do
    char '"'
    content <- many $ noneOf ("\"" :: String)
    char '"'
    pure $ T.pack content

pInteger :: Parser Integer
pInteger = label "integer" $ L.signed (pure ()) L.decimal

pBool :: Parser Bool
pBool = label "bool" $ pTrue <|> pFalse
  where
    pTrue = string "true" $> True
    pFalse = string "false" $> False

pLiteral :: Parser Literal
pLiteral =
  lexeme $
    choice
      [ pString >>= \s -> pure $ LStr s,
        pInteger >>= \i -> pure $ LInt i,
        pBool >>= \b -> pure $ LBool b
      ]

pVar :: Parser Expr
pVar = Var <$> pIdentifier

pLetFn :: Parser Expr
pLetFn =
  label "function" $ do
    symbol "let"
    spaceConsumer
    name <- pIdentifier
    spaceConsumer
    args <- try $ many pIdentifier
    spaceConsumer
    symbol "="
    spaceConsumer
    Fn name args <$> pExpr

pLamb :: Parser Expr
pLamb =
  label "lambda" $
    string "\\" *> spaceConsumer *> do
      param <- pIdentifier
      spaceConsumer
      string "->"
      spaceConsumer
      Lamb param <$> pExpr

pInfixApp :: Parser Expr
pInfixApp = label "infix application" $ do
  expr1 <- pAExpr
  op <- label "operators" $ choice operators
  InfixApp op expr1 <$> pAExpr

operators :: [Parser Op]
operators =
  [ symbol "+" $> Add,
    symbol "-" $> Sub,
    symbol "*" $> Mul,
    symbol "/" $> Div,
    symbol "==" $> Eq,
    symbol "/=" $> Ne,
    symbol "<" $> Lt,
    symbol ">" $> Gt
  ]

pApp :: Parser Expr
pApp =
  label "function application" $ do
    fnExpr <- pAExpr
    args <- many pAExpr
    pure $ foldl App fnExpr args

pExpr :: Parser Expr
pExpr =
  label "expression" $
    choice
      [ pLetFn,
        pLamb,
        try pInfixApp,
        try pApp,
        pAExpr
      ]

pAExpr :: Parser Expr
pAExpr = label "expression in parens or var/literal" $ choice [parens pExpr, pVar, Lit <$> pLiteral]

pStatement :: Parser Expr
pStatement = pExpr <* semicolon

parseFromFile :: FilePath -> IO (Either (ParseErrorBundle Text Void) [Expr])
parseFromFile fileName =
  runParser
    (between spaceConsumer eof (many pStatement))
    fileName
    <$> (readFile fileName <&> T.pack)