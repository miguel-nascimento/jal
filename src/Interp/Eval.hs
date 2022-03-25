module Interp.Eval where

import qualified Data.Map as Map
import Data.Text (Text)
import Syntax.Ast

data Value
  = VLit Literal
  | VClosure Identifier Expr Env
  | VUnit
  deriving (Show)

data EvalError = VariableNotInScope Text | TypeMismatch Text Text
  deriving (Show)

type Env = Map.Map Identifier Value

-- data St' = St'
--   { env :: Env
--   }

-- type Eval = ExceptT EvalError (State St') Value

eval :: [Expr] -> Env -> [Value]
eval [] env = [VUnit]
eval (x : xs) env = case x of
  Var name -> env Map.! name : eval xs env
  Fn name args body ->
    let env' = Map.insert name (VClosure name body env) env
     in eval (body : xs) env'
  Lamb arg body -> _
  App expr1 expr2 -> _
  InfixApp op expr1 expr2 -> _
  Lit lit -> VLit lit : eval xs env