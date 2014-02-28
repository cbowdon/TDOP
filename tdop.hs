module TDOP where

import Control.Monad.Identity
import Control.Monad.State
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
    , nud :: Expr
    , led :: Expr -> Expr }

instance Show Symbol where
    show s = "Symbol " ++ name s

type SymbolMap = M.Map Name (String -> Symbol)

findSymbol :: SymbolMap -> Token -> Maybe Symbol
findSymbol sm (Token n v) =
    case M.lookup n sm of
        Just f  -> Just (f v)
        _       -> Nothing

data InputState = InputState
    { tokens :: [Token]
    , symbols :: SymbolMap
    , token :: Maybe Token
    , symbol :: Maybe Symbol }

advance :: InputState -> InputState
advance i0 = InputState
    { tokens = tokens'
    , symbols = symbols i0
    , token = token'
    , symbol = symbol' }
    where
        tokens' = case tokens i0 of
                    (x:xs)  -> xs
                    _       -> []
        token'      = case tokens i0 of
                        (x:xs)  -> Just x
                        _       -> Nothing
        symbol'     = token' >>= findSymbol (symbols i0)

expression :: BindingPrecedence -> StateT InputState Maybe Expr
expression rbp = do
    i0 <- get
    let s0 = symbol i0
    case s0 of
        Nothing -> return Null
        Just s  -> do
            let left = nud s
            put $ advance i0
            i1 <- get
            let s1 = symbol i1
            case s1 of
                Nothing -> return left
                Just s' -> do
                    let lbp' = lbp s'
                    if rbp < lbp'
                    then
                        return . led s' $ left
                    else
                        return left
