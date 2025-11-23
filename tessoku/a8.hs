import Control.Monad (replicateM)
import Data.Array (listArray, Array, (!))

inputIntList :: Int -> IO [[Int]]
inputIntList n = replicateM n $ getLine >>= return . map read .words

getSum :: Array Int (Array Int Int) -> [Int] -> Int
getSum arrs abcds =  case abcds of
    [a,b,c,d] -> foldl (\acc i -> acc + sumw i) 0 [a..c]
          where sumw i =  let ar = arrs!i in ar!d - if b>1 then ar!(b-1) else 0 
    _ -> 0

main :: IO ()
main = do
  [[h,w]] <- inputIntList 1
  xs <- inputIntList h
  [[q]] <- inputIntList 1
  abcds <- inputIntList q
  let arr = listArray (1,h) $ map (listArray (1,w) . scanl1 (+)) xs 
  let sums = map (getSum arr) abcds
  mapM_ print sums
