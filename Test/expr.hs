module Test.Expr where

import qualified Data.Map as M
import Data.Maybe (fromJust)

type Name = String

-- TODO make this a type variable, user-pluggable
data Expr = IntLit Integer
            | FloLit Float
            | Var Name
            | Fun Name Expr Expr
            | Abs Name Expr
            | App Expr Expr
            | Null -- TODO delete this after implementing language
            deriving (Show)

type Env = M.Map Name Value

data Value = IntVal Integer
            | FloVal Float
            | FunVal Env Name Expr
            deriving (Show)

-- Gratefully taken from: http://www.cs.virginia.edu/~wh5a/personal/Transformers.pdf
eval ::  Env -> Expr -> Value
eval _ (IntLit i)   = IntVal i
eval _ (FloLit f)   = FloVal f
eval env (Var n)    = fromJust (M.lookup n env) -- TODO
eval env (Abs n e)  = FunVal env n e
eval env (App x y)  = let   v0 = eval env x
                            v1 = eval env y
                      in case v0 of
                          FunVal env' n body -> eval (M.insert n v1 env') body

testExpr = App (App (Var "+") (IntLit 2)) (FloLit 2.0)
env = undefined
result = eval env testExpr
