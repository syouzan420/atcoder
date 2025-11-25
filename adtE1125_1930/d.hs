import qualified Data.IntMap as M
import Data.IntMap ((!))

main :: IO ()
main = do
  n <- readLn :: IO Int 
  s <- getLine
  let mp = M.fromList (zip [1..n] s)
  let res = map (show . getl mp n 1) [1..(n-1)]
  putStr $ unlines res

getl :: M.IntMap Char -> Int -> Int -> Int -> Int
getl mp n l i 
  | mp!l==mp!(l+i) = 0 
  | otherwise = if n==l+i then 1 else 1+getl mp n (l+1) i
