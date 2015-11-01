import qualified Data.ByteString.Char8 as C
import Data.Maybe
import Control.Monad

-- Grid City
--
-- Divide the grid into subgrids and calculate them.

triangle 1 = 2
triangle 2 = 5
triangle n
    | odd n     = 2 * (triangle(half) + (n - half))
    | otherwise = triangle(half) + triangle(half-1) + 2 * (n - half) + 1
    where half = n `div` 2


main = do
    t <- fst . fromJust . C.readInt <$> C.getLine
    replicateM_ t $ do
        side <- fst . fromJust . C.readInteger <$> C.getLine
        putStrLn $ show (triangle side)
