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
	, nud = FloLit . read $ n
	, led = const Null }
mkInt n = Symbol
    { name = "Int " ++ show n
	, lbp = 0
	, nud = IntLit . read $ n
	, led = const Null }
mkNoOp n = Symbol
    { name = "NoOp " ++ show n
	, lbp = 0
	, nud = Null
	, led = const Null }
mkIdentifier n = Symbol
    { name = "Identifier " ++ show n
	, lbp = 0
	, nud = Null
	, led = const Null }
mkOperator "+" = Symbol
    { name = "Operator +"
	, lbp = 50
	, nud = Null
	, led = const Null }
mkOperator "*" = Symbol
    { name = "Operator *"
	, lbp = 60
	, nud = Null
	, led = const Null }
mkEnd = const Symbol
    { name = "End"
    , lbp = 0
    , nud = Null
    , led = const Null }

symbolMap :: SymbolMap
symbolMap = M.insert "operator" mkOperator .
            M.insert "float" mkFloat .
            M.insert "int" mkInt .
            M.insert "identifier" mkIdentifier .
            M.insert "whitespace" mkNoOp $
            M.insert "end" mkEnd
            M.empty

result = case readTokens symbolMap tokens of
    Left error  -> undefined
    Right st    -> runIdentity $ runStateT (expression 0) st
