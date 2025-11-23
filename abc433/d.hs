import qualified Data.ByteString.Char8 as B
import Data.List (unfoldr)
import qualified Data.IntMap as M
import Data.IntMap ((!))

ints :: IO [Int]
ints = unfoldr (B.readInt . B.dropSpace) <$> B.getLine

main :: IO ()
main = do
  [n,m] <- ints
  as <- ints
  let mp = M.fromList (zip [1..] as)
  let res = sum [f m (mp!i) (mp!j) |i<-[1..n],j<-[i..n]]
  print res

f :: Int -> Int -> Int -> Int 
f m x y 
  | x == y = let ia = read (show x ++ show y) `mod` m == 0
              in if ia then 1 else 0
  | otherwise = let ia = read (show x ++ show y) `mod` m == 0
                    ib = read (show y ++ show x) `mod` m == 0
                 in if ia&&ib then 2 else if ia||ib then 1 else 0

