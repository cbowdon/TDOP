module HLex
( Token(..)
, Rule
, Rules
, LexResult
, hlex
, mkRule
) where

import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Text.Regex

type Index = Int
type Name = String
type Rule = (Name, Regex)
type Rules = [Rule]
data Token = Token Name String
            | EndToken
            deriving (Show)

type LexResult = Either String [Token]

type HLex a = ReaderT Rules (StateT Index Identity) a

hlex :: Rules -> String -> LexResult
hlex r s = runIdentity $ evalStateT (runReaderT (hlex' s $ Right []) r) 0

hlex' :: String -> LexResult -> HLex LexResult
hlex' _ err@(Left _)  = return err
hlex' [] (Right tokens) = return . Right . reverse $ EndToken:tokens
hlex' s (Right tokens)  = do
    rules <- ask
    i <- get
    case ruleMatch rules s of
        Nothing             -> return . Left $ "Lex error at char " ++ show i
        Just (name, match)  ->  do
            let i'    = length match
                rest  = drop i' s
                token = Token name match
            put (i + i')
            hlex' rest $ Right $ token:tokens

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
