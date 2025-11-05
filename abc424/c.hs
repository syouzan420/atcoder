import Data.Functor ((<&>))
import Control.Monad (replicateM)
import Data.List (delete)

main :: IO ()
main = do
  n <- readLn 
  ab <- replicateM n $ getLine <&> map read . words
  let sn = skn 0 ab 
  let res = show sn 
  putStrLn res

skn :: Int -> [[Int]] -> Int
skn n [] = n
skn n ab = let (nn,nab) = foldl skillNum (n,ab) ab
            in if n==nn then n else skn nn nab

skillNum :: (Int,[[Int]]) -> [Int] -> (Int,[[Int]])
skillNum (n,ab) [a,b]
  | a==0 && b==0 = (n+1,delete [a,b] ab)
  | a==n || b==n = (n+1,delete [a,b] ab)
  | otherwise = (n,ab)
skillNum _ _ = (0,[])

