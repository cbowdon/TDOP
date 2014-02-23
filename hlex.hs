module HLex

import Text.Regex

type Input = String
type RuleName = String
type RuleMap = [(RuleName, Regex)]

matchRule :: Input -> RuleMap -> Maybe RuleName
matchRule input rules = foldr matcher Nothing rules
    where
        matcher result@(Just _) _ = result
        matcher Nothing (name, regex) = matchRegex regex input
