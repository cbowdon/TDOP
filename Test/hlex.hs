module Test.HLex where

import Text.Regex
import HLex

testRules = [ ("operator", mkRegex "([-\\+\\*/])")
            , ("int", mkRegex "([0-9]+)")
            , ("float", mkRegex "([0-9]+.[0-9]+)")
            , ("whitespace", mkRegex "(\\s+)") ]
