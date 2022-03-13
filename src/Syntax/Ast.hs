module Syntax.Ast where

import Data.Text (Text)

type Name = String

data Expr
  = Var Name
  | Lamb Name Expr
  | App Expr Expr
  | Lit Literal
  deriving (Show)

data Literal
  = LStr Text
  | LInt Integer
  | LBool Bool
  deriving (Show)
