{-# LANGUAGE TupleSections #-}
import Control.Monad (replicateM)
import qualified Data.IntMap as M
import Data.IntMap ((!))

main :: IO ()
main = do
  n <- readLn :: IO Int
  as <- replicateM n (readLn :: IO Int)
  let mp = M.fromList $ map (,0) [1..n]
  let nmp = check mp as 
  let x = M.keys $ M.filter (==0) nmp
  let y = M.keys $ M.filter (==2) nmp
  let res = if null x then "Correct" else show (head y) ++ " " ++ show (head x)
  putStrLn res 

check :: M.IntMap Int -> [Int] -> M.IntMap Int 
check mp [] = mp 
check mp (x:xs) = check (M.update (\i -> Just (i+1)) x mp) xs
