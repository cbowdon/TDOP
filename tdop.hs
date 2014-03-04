module TDOP
( Symbol(..)
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

data Symbol a = Symbol
    { name :: Name
    , lbp :: BindingPrecedence
    , nud :: TDOP a
    , led :: a -> TDOP a }

instance Show (Symbol a) where
    show s = "Symbol " ++ name s

type SymbolMap a = M.Map Name (String -> Symbol a)

data InputState a = InputState
    { symbols :: [Symbol a]
    , symbol :: Symbol a }
    deriving (Show)

type TDOP a = StateT (InputState a) Identity a

findSymbol :: SymbolMap a -> Token -> Either String (Symbol a)
findSymbol sm (Token n v) =
    case M.lookup n sm of
        Just f  -> return $ f v
        _       -> Left $ "Symbol not found: " ++ show n

inputState :: [Symbol a] -> InputState a
inputState s = InputState { symbols = drop 1 s, symbol = head s }

readTokens :: SymbolMap a -> [Token] -> Either String (InputState a)
readTokens sm t = do
    s <- mapM (findSymbol sm) t
    return $ inputState s

advance :: StateT (InputState a) Identity ()
advance = do
    i0 <- get
    put $ advance' i0
    where
        advance' i0 = InputState
            { symbols = drop 1 . symbols $ i0
            , symbol = head . symbols $ i0 }

expression :: BindingPrecedence -> TDOP a
expression rbp = do
    i0 <- get
    let s0 = symbol i0
    left <- nud s0
    advance
    expression' rbp left

expression' :: BindingPrecedence -> a -> TDOP a
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
