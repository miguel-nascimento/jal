module Syntax.Parser where

import Data.Functor (($>))
import qualified Data.Text as T
import Data.Void
import Syntax.Ast
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 (L.skipLineComment "--") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

semicolon :: Parser String
semicolon = symbol ";"

pIdentifier :: Parser Identifier
pIdentifier = lexeme $
  label "identifier" $ do
    first <- letterChar
    rest <- many $ alphaNumChar <|> char '_' <|> char '\''
    pure (first : rest)

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

pLiteral :: Parser Literal
pLiteral =
  lexeme $
    choice
      [ pString >>= \s -> pure $ LStr (T.pack s),
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
      args <- pIdentifier
      spaceConsumer
      string "->"
      spaceConsumer
      Lamb args <$> pExpr

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
    symbol "!=" $> Neq,
    symbol "<" $> Lt,
    symbol ">" $> Gt,
    symbol "<=" $> Le,
    symbol ">=" $> Ge
  ]

pApp :: Parser Expr
pApp =
  label "function application" $ do
    expr1 <- pAExpr
    args <- many pAExpr
    pure $ foldl App expr1 args

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

parseFile :: String -> String -> Either String [Expr]
parseFile input filename =
  let outputE =
        parse
          (between spaceConsumer eof (many pStatement))
          filename
          input
   in case outputE of
        Left err -> Left $ errorBundlePretty err
        Right output -> Right output

test :: FilePath -> IO ()
test fileName = do
  fileContent <- readFile fileName
  let ast = parseFile fileContent fileName
  case ast of
    Left err -> putStrLn err
    Right ast' -> print ast'