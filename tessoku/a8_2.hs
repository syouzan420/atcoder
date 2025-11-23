import Control.Monad (replicateM)
import Data.Array (listArray, Array, (!))
import qualified Data.ByteString.Char8 as B
import Data.List (unfoldr,foldl')

ints :: IO [Int]
ints = unfoldr (B.readInt . B.dropSpace) <$> B.getLine

main :: IO ()
main = do
  [h,w] <- ints 
  xs <- replicateM h ints 
  q <- readLn :: IO Int 
  abcds <- replicateM q ints 
  let arr = listArray ((1,1),(h,w)) $ concatMap (scanl1 (+)) xs 
  let sums = map (getSum arr) abcds
  mapM_ print sums
  
getSum :: Array (Int,Int) Int -> [Int] -> Int
getSum ar [a,b,c,d] = foldl' (\acc h -> acc + sumw h) 0 [a..c]
          where sumw h = ar!(h,d) - if b==1 then 0 else ar!(h,b-1) 

