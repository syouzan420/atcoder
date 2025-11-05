
import qualified Data.IntMap as M 

ints :: IO [Int]
ints = map read . words <$> getLine

main :: IO ()
main = do
  [n,q] <- ints
  xs <- ints
  let mp = M.fromList (map (,0) [1..n])
  let res = putBall n mp [] xs
  putStrLn $ unwords $ map show res

putBall :: Int -> M.IntMap Int -> [Int] -> [Int] -> [Int]
putBall _ _ acc [] = acc 
putBall n mp acc (x:xs)
  | x > 0 = putBall n (M.update (\i -> Just (i+1)) x mp) (acc++[x]) xs
  | otherwise = let (box,nmp) = searchBox mp x 0
                 in putBall n nmp (acc++[box]) xs

searchBox :: M.IntMap Int -> Int -> Int -> (Int,M.IntMap Int)
searchBox mp x i = 
  let rmp = M.filter (==i) mp
   in if M.null rmp then searchBox mp x (i+1)
                    else let (hkey,_) = M.findMin rmp
                             nmp = M.update (\j->Just(j+1)) hkey mp
                          in (hkey,nmp)
