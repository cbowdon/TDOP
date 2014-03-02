module TDOP
( Expr(..)
, Symbol(..)
, SymbolMap
, readTokens
, InputState(..)
, inputState
, expression
) where

import Control.Monad.Identity
import Control.Monad.State
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
    , nud :: TDOP Expr
    , led :: Expr -> TDOP Expr }

instance Show Symbol where
    show s = "Symbol " ++ name s

type SymbolMap = M.Map Name (String -> Symbol)

data InputState = InputState
    { symbols :: [Symbol]
    , symbol :: Symbol }
    deriving Show

type TDOP = StateT InputState Identity

findSymbol :: SymbolMap -> Token -> Either String Symbol
findSymbol sm (Token n v) =
    case M.lookup n sm of
        Just f  -> return $ f v
        _       -> Left $ "Symbol not found: " ++ show n

inputState :: [Symbol] -> InputState
inputState s = InputState { symbols = drop 1 s, symbol = head s }

readTokens :: SymbolMap -> [Token] -> Either String InputState
readTokens sm t = do
    s <- mapM (findSymbol sm) t
    return $ inputState s

advance :: StateT InputState Identity ()
advance = do
    i0 <- get
    put $ advance' i0
    where
        advance' i0 = InputState
            { symbols = drop 1 . symbols $ i0
            , symbol = head . symbols $ i0 }

expression :: BindingPrecedence -> TDOP Expr
expression rbp = do
    i0 <- get
    let s0 = symbol i0
    left <- nud s0
    advance
    expression' rbp left

expression' :: BindingPrecedence -> Expr -> TDOP Expr
expression' rbp left = do
    i <- get
    let s = symbol i
    if rbp < lbp s
    then do
        advance
        right <- led s left
        expression' rbp right
    else
        return left
