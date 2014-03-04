module Test.HLex where

-- TODO why does cabal think hunit/quickcheck not installed?
import HLex

rules :: [Rule]
rules = [ mkRule "operator"     "[\\*\\+]"
        , mkRule "int"          "[0-9]+"
        , mkRule "float"        "[0-9]+.[0-9]+"
        , mkRule "whitespace"   "[ \\t]+"
        , mkRule "identifer"    "[A-Za-z_$]+" ]

expr :: String
expr = "0.99 * 100 + offset"

expected :: [Token]
expected =  [ Token "float"         "0.99"
            , Token "operator"      "*"
            , Token "int"           "100"
            , Token "operator"      "+"
            , Token "identifier"    "offset"
            , Token "end"           "" ]

result :: Either String [Token]
result = hlex rules expr

badExpr :: String
badExpr = expr ++ " ^"

badResult :: Either String [Token]
badResult = hlex rules badExpr
