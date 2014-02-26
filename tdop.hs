module TDOP where

import qualified Data.Map as M
import HLex

type Name = String
type BindingPrecedence = Integer

data Expr = IntLit Integer
            | FloLit Float
            | Var Name
            | Op Expr Expr
            deriving Show

data Value = IntVal Integer
            | FloVal Float
            | FunVal Name Expr

type Env = M.Map Name Value

nud :: Token Name -> Maybe Expr
nud (Token "operator" "*") = Nothing

led :: Token Name -> Maybe ([Token Name] -> Expr -> Expr)
led = undefined

lbp :: Token Name -> BindingPrecedence
lbp = undefined

expression :: [Token Name] -> BindingPrecedence -> Maybe Expr
expression (t0:t1:ts) rbp = do
    left <- nud t0
    if rbp >= lbp t1 then return left else do
        right <- led t1
        return $ right (t1:ts) left
{-
function expression(rbp) {
    var left, t = token;
    token = tokenizer.next();
    left = t.nud();
    while (rbp < token.lbp) {
        t = token;
        token = tokenizer.next();
        left = t.led(left);
    }
    return left;
}
-}
