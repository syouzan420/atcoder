--import Data.Ix
--import Data.Array (Array)
import Data.Array.IArray (bounds,elems,assocs,listArray,(!),accumArray)
import Data.Array.Unboxed (UArray)
import Control.Monad (replicateM)

ints :: IO [Int]
ints = map read . words <$> getLine 

getMatInt :: Int -> Int -> IO (UArray (Int, Int) Int)
getMatInt h w = 
  listArray ((0,0),(h-1,w-1)) . concat <$> replicateM h ints

main :: IO ()
main = do
  [h,w] <- ints
  mat <- getMatInt h w

  print $ bounds mat

  print $ elems mat

  print $ assocs mat

  print mat

  let rowSums = listArray @UArray (0,h-1) $
                   [sum [mat!(y,x)|x<-[0..(w-1)]] | y<-[0..(h-1)]]

  print rowSums
