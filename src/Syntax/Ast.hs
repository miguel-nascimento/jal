module Syntax.Ast where

import Data.Text (Text)

type Name = String

data Expr
  = Var Name
  | Lamb Name Expr
  | App Expr Expr
  | Lit Literal

data Literal = LStr Text | LInt Integer
