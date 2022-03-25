module Syntax.Ast where

import Data.Text (Text)

type Identifier = Text

data Expr
  = Var Identifier
  | Fn Identifier [Identifier] Expr
  | Lamb Identifier Expr
  | App Expr Expr
  | InfixApp Op Expr Expr
  | Lit Literal
  deriving (Show)

data Literal
  = LStr Text
  | LInt Integer
  | LBool Bool
  deriving (Show)

data Op = Add | Sub | Mul | Div | Eq | Ne | Lt | Gt
  deriving (Show)