module Test.TDOP where

import qualified Data.Map as M
import HLex
import TDOP
import Test.HLex (rules, expr)

{-
tokens :: Either String [Token]
tokens = hlex rules expr
-}
tokens =    [ Token "float"         "0.99"
            , Token "operator"      "*"
            , Token "int"           "100"
            , Token "operator"      "+"
            , Token "identifier"    "offset"
            , EndToken ]

nada _ _ = Null

mkFloat n = Symbol
    { name = "Float " ++ show n
	, lbp = 0
	, nud = Just . FloLit . read $ n
	, led = Just nada }
mkInt n = Symbol
    { name = "Int " ++ show n
	, lbp = 0
	, nud = Just . IntLit . read $ n
	, led = Just nada }
mkNoOp n = Symbol
    { name = "NoOp " ++ show n
	, lbp = 0
	, nud = Just Null
	, led = Just nada }
mkIdentifier n = Symbol
    { name = "Identifier " ++ show n
	, lbp = 0
	, nud = Just Null
	, led = Just nada }
mkOperator "+" = Symbol
    { name = "Operator +"
	, lbp = 50
	, nud = Just Null
	, led = Just nada }
mkOperator "*" = Symbol
    { name = "Operator *"
	, lbp = 60
	, nud = Just Null
	, led = Just $ \(t0:ts) left -> left + expression symbols ts 60 }

symbols :: M.Map Name (String -> Symbol)
symbols =   M.insert "operator" mkOperator .
            M.insert "float" mkFloat .
            M.insert "int" mkFloat .
            M.insert "identifier" mkIdentifier .
            M.insert "whitespace" mkNoOp $
            M.empty

-- result :: Maybe Expr
result = expression symbols tokens 0
