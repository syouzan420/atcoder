import qualified Data.ByteString.Char8 as B
import Data.List (unfoldr)

ints :: IO [Int]
ints = unfoldr (B.readInt . B.dropSpace) <$> B.getLine

main :: IO ()
main = do
  [_,x,y] <- ints
  as <- ints
  let mina = minimum as
  let res = sumup x y mina 0 as 
  print res 

sumup :: Int -> Int -> Int -> Integer -> [Int] -> Integer
sumup _ _ _ acc [] = acc 
sumup x y mina acc (a:as) = 
  let gmx = getMax x y a mina 
   in if gmx<0 then -1 
               else sumup x y mina (acc+fromIntegral gmx) as

getMax :: Int -> Int -> Int -> Int -> Int
getMax x y a m
  | a == m = m
  | otherwise = let dif = (a-m)*y 
                    cdv = dif `mod` (y-x)
                 in if cdv==0 then a - (dif `div` (y-x)) else -1

