module Test.Expr where

import qualified Data.Map as M
import Data.Maybe (fromJust)

type Name = String

-- TODO make this a type variable, user-pluggable
data Expr = FloLit Float
            | Var Name
            | Plus Expr Expr
            | Multiply Expr Expr
            | Abs Name Expr
            | App Expr Expr
            | Null -- TODO delete this after implementing language
            deriving (Show)

type Env = M.Map Name Value

data Value = FloVal Float
            | FunVal Env Name Expr
            deriving (Show)

-- Gratefully taken from: http://www.cs.virginia.edu/~wh5a/personal/Transformers.pdf
-- TODO actually use the transformers
eval ::  Env -> Expr -> Value
eval _ (FloLit f)   = FloVal f
eval env (Plus x y)     =   let FloVal x' = eval env x
                                FloVal y' = eval env y
                            in  FloVal $ x' + y'
eval env (Multiply x y) =   let FloVal x' = eval env x
                                FloVal y' = eval env y
                            in  FloVal $ x' * y'
eval env (Var n)    = fromJust (M.lookup n env) -- TODO
eval env (Abs n e)  = FunVal env n e
eval env (App x y)  = let   v0 = eval env x
                            v1 = eval env y
                      in case v0 of
                          FunVal env' n body -> eval (M.insert n v1 env') body

testExpr = FloLit 12 `Plus` App (Abs "x" (Var "x")) (FloLit 4 `Plus` FloLit 2)
evalResult = eval M.empty testExpr
