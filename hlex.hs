module HLex where

import Text.Regex

type Input = String
type RuleName = String
type Rules = [(RuleName, Regex)]

matchRule :: Input -> Rules -> Maybe (RuleName, String)
matchRule input = foldr matcher Nothing
    where
        matcher (name, regex) (Just (n1,r1)) =
            case matchRegex regex input of
                Just [r2]   -> Just $ if length r1 >= length r2 then (n1,r1) else (name, r2)
                Just (r2:_) -> Just $ if length r1 >= length r2 then (n1,r1) else (name, r2)
                _           -> Nothing
        matcher (name, regex) Nothing
            = case matchRegex regex input of
                Just [x]    -> Just (name, x)
                Just (x:_)  -> Just (name, x)
                _           -> Nothing


