module HLex where

import Text.Regex

type Input = String
type RuleName = String
type Rules = [(RuleName, Regex)]

matchRule :: Input -> Rules -> Maybe (RuleName, String)
matchRule input = foldr matcher Nothing
    where
        matcher _ result@(Just _)       = result
        matcher (name, regex) Nothing   = case matchRegex regex input of
                                            Just [x]    -> Just (name, x)
                                            Just (x:_)  -> Just (name, x)
                                            _           -> Nothing
