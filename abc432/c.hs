import qualified Data.ByteString.Char8 as B
import Data.List (unfoldr)

ints :: IO [Int]
ints = unfoldr (B.readInt . B.dropSpace) <$> B.getLine

main :: IO ()
main = do
  [_,x,y] <- ints
  as <- ints
  let mg = y * minimum as
  let res = sumup x y mg 0 as 
  print res 

sumup :: Int -> Int -> Int -> Integer -> [Int] -> Integer
sumup _ _ _ acc [] = acc 
sumup x y mg acc (a:as) = 
  let gmx = getMax x y mg a a
   in if gmx<0 then fromIntegral gmx 
               else sumup x y mg (acc+fromIntegral gmx) as

getMax :: Int -> Int -> Int -> Int -> Int -> Int
getMax _ _ _ _ 0 = -1 
getMax x y mg a m
  | m==a-1 = if g==mg then m else -1 
  | g==mg = m
  | g>mg = getMax x y mg a (m `div` 2) 
  | otherwise = getMax x y mg a ((m+a) `div` 2)
  where g = x*(a-m)+y*m

