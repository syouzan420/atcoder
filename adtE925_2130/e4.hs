import Data.Functor ((<&>))
import qualified Data.IntMap as M
import Data.IntMap ((!))

type MP = M.IntMap Int

ints :: IO [Int]
ints = getLine <&> map read . words

main :: IO ()
main = do
  n <- readLn 
  ss <- ints
  ts <- ints
  let ar = M.fromList $ zip [0..n] (scanl1 (+) (0:ss))  
  let tar = M.fromList $ zip [1..n] ts
  let res = map (minDist n ar tar) [1..n] 
  mapM_ print res  

minDist :: Int -> MP -> MP -> Int -> Int
minDist n ar tar i = minimum $ (tar!i):distsTo n i ar tar

distsTo :: Int -> Int -> MP -> MP -> [Int]
distsTo n i ar tar = map (\j -> let tj=tar!j in if tj > tar!i then tj else tj + dist n j i ar) ([(i+1)..n]++[1..(i-1)])

dist :: Int -> Int -> Int -> MP -> Int
dist n s e ar 
  | e > s = ar!(e-1) - ar!(s-1) 
  | otherwise = ar!n - ar!(s-1) + ar!(e-1)

