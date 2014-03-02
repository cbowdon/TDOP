module Test.TDOP where

import Control.Monad.Identity
import Control.Monad.State
import qualified Data.Map as M
import HLex
import TDOP

tokens =    [ Token "float"         "0.99"
            , Token "operator"      "*"
            , Token "int"           "100"
            , Token "operator"      "+"
            , Token "identifier"    "offset"
            , Token "end"           "" ]

mkFloat n = Symbol
    { name = "Float " ++ show n
	, lbp = 0
	, nud = return . FloLit . read $ n
	, led = const . return $ Null }
mkInt n = Symbol
    { name = "Int " ++ show n
	, lbp = 0
	, nud = return . IntLit . read $ n
	, led = const . return $ Null }
mkNoOp n = Symbol
    { name = "NoOp " ++ show n
	, lbp = 0
	, nud = return Null
	, led = const . return $ Null }
mkIdentifier n = Symbol
    { name = "Identifier " ++ show n
	, lbp = 0
	, nud = return Null
	, led = const . return $ Null }
mkOperator "+" = Symbol
    { name = "Operator +"
	, lbp = 50
	, nud = return Null
	, led = const . return $ Null }
mkOperator "*" = Symbol
    { name = "Operator *"
	, lbp = 60
	, nud = return Null
	, led = const . return $ Null }
mkEnd = const Symbol
    { name = "End"
    , lbp = 0
    , nud = return Null
    , led = const . return $ Null }

symbolMap :: SymbolMap
symbolMap = M.fromList  [ ("operator", mkOperator)
                        , ("float", mkFloat)
                        , ("int", mkInt)
                        , ("identifier", mkIdentifier)
                        , ("whitespace", mkNoOp)
                        , ("end", mkEnd) ]

result = case readTokens symbolMap tokens of
    Left error  -> undefined
    Right st    -> runIdentity $ runStateT (expression 0) st
