import qualified Data.IntMap as M
import Data.IntMap ((!))
import Data.List (scanl')

ints :: IO [Int]
ints = map read . words <$> getLine

main :: IO ()
main = do
  [n,k] <- ints
  as <- ints
  let mp = M.fromList $ zip [0..(n+1)] (scanl' (+) 0 as)
  let res = solve k as
  print res

check :: Int -> Int -> Int -> Int -> M.IntMap Int -> Int
check n l r k mp
  | l==r = n-r 
  | otherwise = let m = (l+r) `div` 2
                    p = mp!m - mp!(l-1)
                 in if p>=k then check n l m k mp else check n m r k mp
              

solve :: Int -> [Int] -> Int
solve = undefined
