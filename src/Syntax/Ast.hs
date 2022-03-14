module Syntax.Ast where

import Data.Text (Text)

type Identifier = String

data Expr
  = Var Identifier
  | Fn Identifier [Identifier] Expr
  | Lamb Identifier Expr
  | App Expr Expr
  | OpApp Op Expr Expr
  | Op Op
  | Lit Literal
  deriving (Show)

data Literal
  = LStr Text
  | LInt Integer
  | LBool Bool
  deriving (Show)

data Op = Add | Sub | Mul | Div | Eq | Neq | Lt | Gt | Le | Ge
  deriving (Show)