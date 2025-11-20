import Control.Monad (replicateM)
import qualified Data.IntMap as M
import Data.IntMap ((!))

ints :: IO [Int]
ints = map read . words <$> getLine

main :: IO ()
main = do
  d <- readLn :: IO Int
  n <- readLn :: IO Int
  lrs <- replicateM n ints
  let mp = M.fromList (map (,0) [1..(d+1)]) 
  let nmp = foldl updateMap mp lrs
  mapM_ print $ init $ scanl1 (+) (M.elems nmp)

updateMap :: M.IntMap Int -> [Int] -> M.IntMap Int
updateMap mp [l,r] = upd (+(-1)) (r+1) $ upd (+1) l mp
    where upd f = M.update (Just . f) 
   
