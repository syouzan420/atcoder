import Control.Monad (replicateM)
import Data.List (scanl')
import qualified Data.IntMap as M
import Data.IntMap ((!))

ints :: IO [Int]
ints = map read . words <$> getLine

main :: IO ()
main = do
  x <- readLn :: IO Int
  n <- readLn :: IO Int
  ws <- ints
  q <- readLn :: IO Int
  ps <- replicateM q (readLn :: IO Int)
  let mp = M.fromList (map (,False) [1..n])
  let res = map snd $ scanl' (change ws) (mp,x) ps 
  putStr $ unlines (map show (tail res))

change :: [Int] -> (M.IntMap Bool,Int) -> Int -> (M.IntMap Bool,Int)
change ws (mp,x) p = let bl = mp!p 
                      in if bl then (M.update (\_->Just False) p mp,x-ws!!(p-1))
                               else (M.update (\_->Just True) p mp,x+ws!!(p-1))
