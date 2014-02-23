module HLex where

import Control.Monad
import Control.Monad.Reader
import Text.Regex

type Index = Int
type Name = String
type Rule = (Name, Regex)
type Rules = [Rule]
data Token =
            Token Name String
            | EndToken
            deriving (Show)

hlex :: Rules -> String -> Either String [Token]
hlex r s = hlex' r s 0 $ Right []

hlex' :: Rules -> String -> Index -> Either String [Token] -> Either String [Token]
hlex' r _ _ error@(Left _) = error
hlex' r [] _ (Right tokens) = Right . reverse $ EndToken:tokens
hlex' r s i (Right tokens) =
    case ruleMatch r s of
        Nothing             -> Left $ "Lex error at char " ++ show i
        Just (name, match)  ->  let i'    = length match
                                    rem   = drop i' s
                                    token = Token name match
                                in hlex' r rem (i + i') $ Right $ token:tokens

mkRule :: Name -> String -> Rule
mkRule n s = (n, mkRegex $ "^(" ++ s ++ ")")

ruleMatch :: Rules -> String -> Maybe (Name, String)
ruleMatch rules input = foldr matcher Nothing rules
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
