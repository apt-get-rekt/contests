import qualified Data.ByteString.Char8 as C
import Data.List
import Data.Maybe
import Control.Monad
import Data.Ord

-- Grid City
-- Dynamic Programming
--
-- Divide the grid into subgrids and calculate them.
-- Each grid and subgrid is a triangle

-- Calculate triangle
triangle n calculated
    | isJust preCalc = fromJust preCalc
    | odd n          =  2 * (triangle half calculated + (n - half))
    | otherwise      = triangle half calculated + triangle (half - 1) calculated + 2 * (n - half) + 1
    where half    = n `div` 2
          preCalc = lookup n calculated

-- Creates a list of the triangles that have not been calculated
dynamicTriangleAux n calculated
    | isJust preCalc = []
    | odd n          = recursive half ++ [n]
    | otherwise      = dynamicTriangleAux (half-1) calculated ++ recursive half ++ [n]
    where half           = n `div` 2
          preCalc        = lookup n calculated
          recursive half = dynamicTriangleAux half calculated

-- Calculates new values and saves them as memorization
dynamicTriangle [] newCalculated = newCalculated
dynamicTriangle toCalculate@(h:t) calculated = dynamicTriangle t newCalculated
    where newCalculated = insertBy (comparing fst) (h, triangle h calculated) calculated

-- Calculates a series of triangles through dynamic programming
triangles [] calculated = calculated
triangles (h:t) calculated = triangles t newCalculated
    where toCalculate   = nub $ dynamicTriangleAux h calculated
          newCalculated = dynamicTriangle toCalculate calculated

-- Filter solutions for the answers to the input
giveAnswers [] _ = []
giveAnswers (input : t) solutions = answer : giveAnswers t solutions
    where answer = fromJust $ lookup input solutions

main = do
    t <- fst . fromJust . C.readInt <$> C.getLine
    input <- replicateM t $ do
         fst . fromJust . C.readInteger <$> C.getLine
    let solutions = triangles input [(1,2),(2,5)]
    putStrLn . unlines $ map show (giveAnswers input solutions)
