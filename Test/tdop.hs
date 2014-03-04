module Test.TDOP
( result
) where

import Control.Monad.Identity
import Control.Monad.State
import qualified Data.Map as M
import HLex
import TDOP
import Test.Expr

tokens :: [Token]
tokens =    [ Token "float"         "0.99"
            , Token "operator"      "*"
            , Token "int"           "100"
            , Token "operator"      "+"
            , Token "identifier"    "offset"
            , Token "end"           "" ]

mkOperator :: String -> Symbol Expr
mkOperator "+" = Symbol
    { name = "Operator +"
	, lbp = 50
	, nud = return Null
	, led = \left -> do
        right <- expression 50
        return $ Fun "+" left right }

mkOperator "*" = Symbol
    { name = "Operator *"
	, lbp = 60
	, nud = return Null
	, led = \left -> do
        right <- expression 60
        return $ Fun "*" left right }

mkFloat :: String -> Symbol Expr
mkFloat n = Symbol
    { name = "Float " ++ show n
	, lbp = 0
	, nud = return . FloLit . read $ n
	, led = const . return $ Null }

mkInt :: String -> Symbol Expr
mkInt n = Symbol
    { name = "Int " ++ show n
	, lbp = 0
	, nud = return . IntLit . read $ n
	, led = const . return $ Null }

mkNoOp :: String -> Symbol Expr
mkNoOp n = Symbol
    { name = "NoOp " ++ show n
	, lbp = 0
	, nud = return Null
	, led = const . return $ Null }

mkIdentifier :: String -> Symbol Expr
mkIdentifier n = Symbol
    { name = "Identifier " ++ show n
	, lbp = 0
	, nud = return $ Var n
	, led = const . return $ Null }

mkEnd :: String -> Symbol Expr
mkEnd = const Symbol
    { name = "End"
    , lbp = 0
    , nud = return Null
    , led = const . return $ Null }

symbolMap :: SymbolMap Expr
symbolMap = M.fromList  [ ("operator", mkOperator)
                        , ("float", mkFloat)
                        , ("int", mkInt)
                        , ("identifier", mkIdentifier)
                        , ("whitespace", mkNoOp)
                        , ("end", mkEnd) ]

result :: (Expr, InputState Expr)
result = case readTokens symbolMap tokens of
    Left _      -> undefined
    Right st    -> runIdentity $ runStateT (expression 0) st
