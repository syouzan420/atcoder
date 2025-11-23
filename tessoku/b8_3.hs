import Control.Monad (replicateM)
import Data.List (transpose,unfoldr,foldl')
import Data.List.Split (chunksOf)
import qualified Data.ByteString.Char8 as B
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed ((//),(!))

type MV = V.Vector Int 

ints :: IO [Int]
ints = unfoldr (B.readInt . B.dropSpace) <$> B.getLine

main :: IO ()
main = do
  let w = 1500; h = 1500
  n <- readLn :: IO Int 
  xys <- replicateM n ints
  q <- readLn :: IO Int 
  abcds <- replicateM q ints 
  let base = V.replicate ((h+1)*(w+1)) 0 
  let ivp = map (\[x,y]->((w+1)*y+x,1)) xys
  let ones = V.accum (+) base ivp 
  let lst = chunksOf (w+1) $ V.toList ones
  let m = V.fromList $ concat $ scanning lst 
  mapM_ (print . calcSum m w) abcds 

calcSum :: MV -> Int -> [Int] -> Int
calcSum m w [a,b,c,d] = 
  let ul = m!((w+1)*a+b)
      ur = m!((w+1)*(c+1)+b)
      dl = m!((w+1)*a+d+1)
      dr = m!((w+1)*(c+1)+d+1)
   in ul+dr-ur-dl 
  
scanning :: [[Int]] -> [[Int]]
scanning = scanPlus . scanPlus
  where scanPlus = transpose . map (scanl1 (+))

