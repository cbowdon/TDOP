module Test.HLex where

import Text.Regex
import HLex

rules = [ mkRule "operator"     "[\\*\\+]"
        , mkRule "int"          "[0-9]+"
        , mkRule "float"        "[0-9]+.[0-9]+"
        , mkRule "whitespace"   "[ \\t]+"
        , mkRule "identifer"    "[A-Za-z_$]+" ]

expr = "0.99 * 100 + offset"

expected =  [ Token "float"         "0.99"
            , Token "operator"      "*"
            , Token "int"           "100"
            , Token "operator"      "+"
            , Token "identifier"    "offset"
            , EndToken ]

result = hlex rules expr

badExpr = expr ++ " ^"

badResult = hlex rules badExpr
