module HLex where

import Text.Regex

type Input = String
type RuleName = String
type Rules = [(RuleName, Regex)]

ruleMatch :: Input -> Rules -> Maybe (RuleName, String)
ruleMatch input = foldr matcher Nothing
    where
        matcher (n', regex) (Just (n, r)) =
            case matchRegex regex input of
                Just [r']   -> Just $ if length r >= length r' then (n, r) else (n', r')
                Just (r':_) -> Just $ if length r >= length r' then (n, r) else (n', r')
                _           -> Just (n, r)
        matcher (n, regex) Nothing
            = case matchRegex regex input of
                Just [x]    -> Just (n, x)
                Just (x:_)  -> Just (n, x)
                _           -> Nothing
