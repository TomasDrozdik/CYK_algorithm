import CFG
import Data.Array -- for Array
import Data.List  -- for nub

type CYKMatrix a = Array (Int, Int) a

cyk' :: CFG -> String -> Array (Int, Int) [NT]
-- ^ creates cyk matrix based on cyk algorithm
cyk' cfg s =
    let n = -1 + length s
        m = array ((0,0), (n, n)) $
          [ ((x, x + i), generators (x, (x + i))) | i <- [0..n],
                                                    x <- [0..n-i] ] ++  -- upper triangular + diagonal
          [ ((x, y), []) | x <- [0..n],                      
                           y <- [0..n],
                           x > y]                                       -- lower triangular

            where generators :: (Int, Int) -> [NT]
                  -- ^ returns NTs which generate string indexed from x to y
                  generators (x, y) =
                    if x == y then termGens cfg [s!!x]                  -- diagonal only direct rules
                    else nub $ concat $ [ntGens' a b | t <- [0..y - 1],
                                                       a <- m ! (x, x + t),
                                                       b <- m ! (x + t + 1, y)]
                        
                        where ntGens' :: [NT] -> [NT] -> [NT]
                              -- ^ takes two lists of NTs and returns list of NT generating 
                              --   ordered combinations of original lists
                              ntGens' xs ys = concat $ concat $ map 
                                  (\x -> map (\y -> ntGens cfg (x, y)) ys) xs

    in m


cyk :: CFG -> String -> Bool
-- ^ CYK algorithm for CFG grammar in CHNF parsing String s
cyk cfg s =
    "S" `elem` (cyk' cfg s) ! (0, (length s) - 1)


main = do
    putStrLn "--- CYK algorithm ---"
    putStrLn "Name of file with CFG in Chomsky Normal Form:"
    file <- getLine
    cfg <- grammarFromFile file
    putStrLn "Insert the string to parse:"
    print $ fmap (cyk cfg) getLine
