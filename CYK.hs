import CFG
import Data.Array -- for Array
import Data.List  -- for nub

type CYKMatrix a = Array (Int, Int) a

cyk' :: CFG -> String -> CYKMatrix [NT]
-- ^ creates cyk matrix based on cyk algorithm
cyk' cfg s =
    let n = -1 + length s
        m = array ((0,0), (n, n)) $
          [ ((x, x + i), generators x (x + i)) | i <- [0..n],
                                                 x <- [0..n-i] ] ++  -- upper triangular + diagonal
          [ ((x, y), []) | x <- [0..n],                      
                           y <- [0..n],
                           x > y]                                    -- lower triangular - empty

            where generators :: Int -> Int -> [NT]
                  -- ^ returns NTs which generate string from index x to y
                  generators x y =
                    if x == y then termGens cfg [s!!x]               -- diagonal only direct rules
                    else nub $ concat $ [ntGens cfg (a, b) | t <- [x..y - 1],
                                                             a <- m ! (x, t),
                                                             b <- m ! (t + 1, y)]
    in m


cyk :: CFG -> String -> Bool
-- ^ CYK algorithm for CFG grammar in CHNF parsing String s 
-- returns True if cfg =>* s
cyk cfg s =
    "S" `elem` (cyk' cfg s) ! (0, (length s) - 1)


main = do
    putStrLn "           --- CYK algorithm ---" 
    putStrLn "\nName of file with CFG in Chomsky Normal Form:"
    file <- getLine
    cfg <- grammarFromFile file
    putStrLn "\nInsert the string to parse:"
    s <- getLine
    print $ cyk cfg s
