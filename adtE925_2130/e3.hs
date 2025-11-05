import Data.Functor ((<&>))
import Data.Array (Array,listArray,(!))
import qualified Data.ByteString.Char8 as B
import Data.List (unfoldr)

ints :: IO [Int]
ints = unfoldr (B.readInt . B.dropSpace) <$> B.getLine 

main :: IO ()
main = do
  n <- readLn 
  ss <- ints
  ts <- ints
  let ar = listArray (0,n) (scanl1 (+) (0:ss))  
  let tar = listArray (1,n) ts
  let res = map (minDist n ar tar) [1..n] 
  mapM_ print res  

minDist :: Int -> Array Int Int -> Array Int Int -> Int -> Int
minDist n ar tar i = minimum $ (tar!i):distsTo n i ar tar

distsTo :: Int -> Int -> Array Int Int -> Array Int Int -> [Int]
distsTo n i ar tar = map (\j -> let tj=tar!j in if tj > tar!i then tj else tj + dist n j i ar) ([(i+1)..n]++[1..(i-1)])

dist :: Int -> Int -> Int -> Array Int Int -> Int
dist n s e ar 
  | e > s = ar!(e-1) - ar!(s-1) 
  | otherwise = ar!n - ar!(s-1) + ar!(e-1)

