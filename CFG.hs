module Grammar where

    type NT = String
    type T  = String
    
    data RightSide = Branch (NT, NT) | Chain T deriving (Show, Eq, Ord)
    -- | A Rule in Chomsky-Normal-Form is either branching (NT -> NTNT), or chaining multiple T (NT -> T^n)
    
    type Rule = (NT, RightSide)
    type Grammar = [Rule]

    leftSide :: Rule -> NT
    leftSide = fst

    rightSide :: Rule -> RightSide
    rightSide = snd

    leftSides :: [Rule] -> [NT]
    leftSides = map leftSide

    rightSides :: [Rule] -> [RightSide]
    rightSides = map rightSide

    rulesFor :: RightSide -> Grammar -> [Rule]
    -- ^ returns all the rules that can derive the given right side
    rulesFor r = filter ((r==) . rightSide)

    branchRulesFor :: (NT, NT) -> Grammar -> [Rule]
    branchRulesFor = rulesFor . Branch

    chainRulesFor :: T -> Grammar -> [Rule]
    chainRulesFor = rulesFor . Chain

    parseRule :: String -> Rule
    -- ^ Takes a string of the form "S -> A B" or "S -> a" and returns a corresponding rule
    parseRule ws = f $ words ws
        where   f [s, _, t] = (s, Chain t)
                f [s, _, a, b] = (s, Branch (a, b))
    
    parseGrammar :: [String] -> Grammar
    -- ^ Takes a list of rule-formatted strings and returns a grammar
    parseGrammar = map parseRule

    grammarFromFile :: String -> IO Grammar
    -- ^ Takes a filename and returns a Grammar, read in from that file.
    grammarFromFile fs = do f <- readFile fs; return $ parseGrammar $ lines f
