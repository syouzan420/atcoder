import Control.Monad (replicateM)
import Data.Array (listArray, elems, Array, accum, (!))
import Data.List (transpose)
import Data.List.Split (chunksOf)

getInts :: Int -> IO [[Int]]
getInts n = replicateM n $ getLine >>= return . map read .words

baseArray :: Int -> Int -> Array (Int,Int) Int 
baseArray w h = listArray ((0,0),(w,h)) $ repeat 0 
  
setOne :: Array (Int,Int) Int -> [(Int,Int)] -> Array (Int,Int) Int  
setOne arr xys = accum (+) arr $ map (\xy -> (xy,1)) xys 

calcSum :: Array (Int,Int) Int -> (Int,Int,Int,Int) -> Int
calcSum arr (a,b,c,d) = 
  let ul = arr!(a-1,b-1)
      ur = arr!(c,b-1)
      dl = arr!(a-1,d)
      dr = arr!(c,d)
   in ul+dr-ur-dl 
  
toList :: Int -> Array (Int,Int) Int -> [[Int]]
toList w arr = chunksOf (w+1) $ elems arr

scanPlus :: [[Int]] -> [[Int]]
scanPlus = transpose . map (scanl1 (+))

scanning :: [[Int]] -> [[Int]]
scanning = scanPlus . scanPlus

makePair :: [Int] -> (Int,Int)
makePair [a,b] = (a,b)
makePair _ = (0,0)

make4 :: [Int] -> (Int,Int,Int,Int)
make4 [a,b,c,d] = (a,b,c,d)
make4 _ = (0,0,0,0)

main :: IO ()
main = do
  let w = 1500; h = 1500
  n <- getLine >>= return . read
  xys <- getInts n >>= return . map makePair
  q <- getLine >>= return . read
  abcds <- getInts q >>= return . map make4
  let arr = listArray ((0,0),(w,h)) $ concat $ scanning $ toList h $ setOne (baseArray w h) xys 
  mapM_ print $ map (calcSum arr) abcds 
