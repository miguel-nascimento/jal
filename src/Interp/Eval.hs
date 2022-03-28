module Interp.Eval where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Syntax.Ast

data Value
  = VLit Literal
  | VClosure Env Identifier Expr
  | VUnit
  deriving (Show)

data EvalError = VariableNotInScope Text | TypeMismatch Text Text
  deriving (Show)

type Env = Map.Map Identifier Value

eval :: Expr -> Env -> Value
eval expr env = case expr of
  Var name -> env Map.! name
  Fn name param body ->
    let env' = Map.insert name (VClosure env name body) env
     in eval body env'
  Lamb param body -> VClosure env param body
  App fn arg ->
    let argValue = eval arg env
        fnValue = eval fn env
     in case fnValue of
          VClosure env param body ->
            let env' = Map.insert param argValue env
             in eval body env'
          _ -> error "Applied non-function"
  InfixApp op expr1 expr2 -> undefined
  Lit lit -> VLit lit
