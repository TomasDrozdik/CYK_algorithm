import Grammar
import Data.Array
import Control.Applicative

type CYKMatrix a = Array (Int, Int) a
-- ^Primary data structure to perform the CYK algorithm

loopArray g a = m
    where m = array bnds $ map f $ assocs a
          bnds = bounds a

