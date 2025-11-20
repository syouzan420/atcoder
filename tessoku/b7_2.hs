import Data.List (unfoldr)
import Data.Array.ST (STUArray)
import Control.Monad (replicateM)
import Control.Monad.ST (ST, runST)
import qualified Data.Array.MArray as A
import qualified Data.ByteString.Char8 as B

ints :: IO [Int]
ints = unfoldr (B.readInt . B.dropSpace) <$> B.getLine

main :: IO ()
main = do
  t <- readLn :: IO Int
  n <- readLn :: IO Int
  lrs <- replicateM n ints
  let arr = aList t lrs
  mapM_ print $ init $ scanl1 (+) arr 

aList :: Int -> [[Int]] -> [Int]
aList t lrs = runST $ do   
  arr <- A.newArray (0,t) 0 :: ST s (STUArray s Int Int)
  mapM_ (\[l,r] -> do
            A.readArray arr l >>= A.writeArray arr l . (+1)
            A.readArray arr r >>= A.writeArray arr r . (+(-1))) lrs
  A.getElems arr
