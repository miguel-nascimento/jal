module Interp.Eval where

import qualified Data.Map as Map
import Syntax.Ast

data Value
  = VLit Literal
  | VClosure Identifier Expr Context
  | VUnit
  deriving (Show)

type Context = Map.Map Identifier Value

eval :: Context -> Expr -> Value
eval ctx (Var name) = ctx Map.! name
eval ctx (Lamb arg body) = VClosure arg body ctx
eval ctx (App expr1 expr2) =
  let r = eval ctx expr1
      r' = eval ctx expr2
   in case r of
        VClosure arg body ctx' -> eval (Map.insert arg r' ctx') body
        VLit lit -> VLit lit
        VUnit -> VUnit
eval ctx (Lit lit) = VLit lit
eval ctx (Fn name args body) = _
eval ctx (InfixApp op expr1 expr2) = _
