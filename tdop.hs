module TDOP where

import Control.Monad.Writer
import qualified Data.Map as M
import HLex

type BindingPrecedence = Integer

data Expr = IntLit Integer
            | FloLit Float
            | Var Name
            | Op Expr Expr
            | Null
            deriving Show

data Value = IntVal Integer
            | FloVal Float
            | FunVal Name Expr

type Env = M.Map Name Value

data Symbol = Symbol
    { name :: Name
    , lbp :: BindingPrecedence
    , nud :: Maybe Expr
    , led :: Maybe ([Token] -> Expr -> Expr) }

instance Show Symbol where
    show s = "Symbol " ++ name s

type SymbolMap = M.Map Name (String -> Symbol)

symbol :: SymbolMap -> Token -> Maybe Symbol
symbol sm (Token n v) =
    case M.lookup n sm of
        Just f  -> Just (f v)
        _       -> Nothing

expression :: SymbolMap -> [Token] -> BindingPrecedence -> WriterT String Maybe Expr
expression sm (t0:t1:ts) rbp = do
    tell $ "t0 = " ++ show t0 ++ ", t1 = " ++ show t1
    s0 <- lift $ symbol sm t0
    s1 <- lift $ symbol sm t1
    left <- lift $ nud s0
    tell $ "left = " ++ show left ++ ", rbp < lbp = " ++ show (rbp < lbp s1)
    if rbp < lbp s1 then do
        right <- lift $ led s1
        let result = right (t1:ts) left
        tell $ ", result == " ++ show result
        return result
    else return left
