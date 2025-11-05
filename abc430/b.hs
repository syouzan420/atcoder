import Control.Monad (replicateM)
import Data.List (nub)

ints :: IO [Int]
ints = map read . words <$> getLine

main :: IO ()
main = do
  [n,m] <- ints
  ss <- replicateM n getLine 
  let res = length $ nub [getPart m ss (x,y)|x<-[0..(n-m)],y<-[0..(n-m)]] 
  print res

getPart :: Int -> [String] -> (Int,Int) -> [String]
getPart m ss (x,y) = let lns = take m $ drop y ss
                      in map (take m . drop x) lns
