import qualified Data.IntMap as M
import Data.IntMap ((!))

main :: IO ()
main = do
  s <- getLine
  let ms = M.fromList (map (,0) [1..26]) 
  let cal = foldl (\nms ch-> 
                M.update (Just . (+1)) (fromEnum ch-96) nms) ms s
  let ck = length s > 26 || check 1 cal 
  let res = (if ck then 1 else 0) + change 1 2 cal 
  print res

check :: Int -> M.IntMap Int -> Bool
check 27 _ = False
check i cal = cal!i>1 || check (i+1) cal 

change :: Int -> Int -> M.IntMap Int -> Int
change i j cal 
  | i>25 = 0
  | j>26 = change (i+1) (i+2) cal 
  | otherwise = cal!i*cal!j+change i (j+1) cal 
