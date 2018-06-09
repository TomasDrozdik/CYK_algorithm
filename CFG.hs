module CFG where

type NT = String
type T  = String

data RightSide = NTs (NT, NT) | Term T deriving (Show, Eq, Ord)

type Rule = (NT, RightSide)
type CFG = [Rule]

leftSide :: Rule -> NT
leftSide = fst

rightSide :: Rule -> RightSide
rightSide = snd

rulesFor :: RightSide -> CFG -> [Rule]
-- ^ returns all the rules that can derive the given right side
rulesFor r = filter ((r==) . rightSide)

ntGens :: CFG -> (NT, NT) -> [NT]
ntGens g r = map leftSide $ rulesFor (NTs r) g

termGens :: CFG -> T -> [NT]
termGens g r = map leftSide $ rulesFor (Term r) g

parseRule :: String -> Rule
-- ^ Takes a string of the form "S -> A B" or "S -> a" and returns a corresponding rule
parseRule ws = f $ words ws 
    where f [nt, "->", t] = (nt, Term t)
          f [nt, "->", nt1, nt2] = (nt, NTs (nt1, nt2))

parseCFG :: [String] -> CFG
-- ^ Takes a list of rule-formatted strings and returns a grammar
parseCFG = map parseRule

grammarFromFile :: String -> IO CFG
-- ^ Takes a filename and returns a CFG, read in from that file.
grammarFromFile fs = do f <- readFile fs; return $ parseCFG $ lines f
