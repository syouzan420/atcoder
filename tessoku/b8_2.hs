import Control.Monad (replicateM)
import Data.Array (listArray, elems, Array, accum, (!))
import Data.List (transpose,unfoldr)
import Data.List.Split (chunksOf)
import qualified Data.ByteString.Char8 as B

type MA = Array (Int,Int) Int

ints :: IO [Int]
ints = unfoldr (B.readInt . B.dropSpace) <$> B.getLine

main :: IO ()
main = do
  let w = 1500; h = 1500
  n <- readLn :: IO Int 
  xys <- replicateM n ints
  q <- readLn :: IO Int 
  abcds <- replicateM q ints 
  let base = listArray ((0,0),(w,h)) $ repeat 0
  let ones = accum (+) base $ map (\[x,y]->((x,y),1)) xys
  let arr = listArray ((0,0),(w,h)) $ concat $ scanning $ toList h ones 
  mapM_ (print . calcSum arr) abcds 

calcSum :: MA -> [Int] -> Int
calcSum arr [a,b,c,d] = 
  let ul = arr!(a-1,b-1)
      ur = arr!(c,b-1)
      dl = arr!(a-1,d)
      dr = arr!(c,d)
   in ul+dr-ur-dl 
  
toList :: Int -> Array (Int,Int) Int -> [[Int]]
toList h arr = chunksOf (h+1) $ elems arr

scanning :: [[Int]] -> [[Int]]
scanning = scanPlus . scanPlus
  where scanPlus = transpose . map (scanl1 (+))

