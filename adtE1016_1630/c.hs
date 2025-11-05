import qualified Data.Map as M
import Data.Map ((!))

ints :: IO [Int]
ints = map read . words <$> getLine

main :: IO ()
main = do
  n <- readLn
  ps <- ints
  let mps = M.fromList (zip [(2::Int)..] ps)
  let gn = check n mps 
  print gn 

check :: Int -> M.Map Int Int -> Int
check x mp 
  | mp!x==1 = 1
  | otherwise = 1 + check (mp!x) mp

