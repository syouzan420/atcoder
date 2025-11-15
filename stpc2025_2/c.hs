import qualified Data.IntMap as M
import Data.IntMap ((!))

type MP = M.IntMap Int

ints :: IO [Int]
ints = map read . words <$> getLine

main :: IO ()
main = do
  [n,x,y,k,m] <- ints
  let cn = count n x y k
  let res = cn `mod` m
  print res

fc :: Int -> Int
fc 1 = 1
fc x = x*fc (x-1)

count :: Int -> Int -> Int -> Int -> Int
count n x y k = let ba = take n $ replicate x 1 ++ replicate y 2 ++ repeat 3
                    mxt = ten n (M.fromList (zip [1..] (reverse ba)))
                 in if mxt==k then fc n `div` (fc x * fc y * fc (n-x-y))
                              else 0

ten :: Int -> MP -> Int
ten = ten' 1 2 

ten' :: Int -> Int -> Int -> MP -> Int
ten' i j n mp 
  | i>(n-1) = 0
  | j>n = ten' (i+1) (i+2) n mp
  | otherwise = if mp!i > mp!j then 1+ ten' i (j+1) n mp
                               else ten' i (j+1) n mp
