import Control.Monad (replicateM)
--import Data.Array (Array,listArray,(!),(//))
import Data.Array.IArray (Array,amap,listArray,(!),(//))
import qualified Data.ByteString.Char8 as B
import Data.List (unfoldr)

type MyArray = Array Int Int

ints :: IO [Int]
ints = unfoldr (B.readInt . B.dropSpace) <$> B.getLine

main :: IO ()
main = do
  [n,q] <- ints 
  as <- ints
  qs <- replicateM q ints
  let sas = scanl1 (+) as
  let ar = listArray (0,n) (0:sas)
  aquery n qs ar 

aquery :: Int -> [[Int]] -> MyArray -> IO () 
aquery _ [] _ = return () 
aquery n (q:qs) ar = do
  let hq = head q 
  if hq==1 then do
      let nar = move (last q) n ar  
      aquery n qs nar 
           else do
      let [l,r] = tail q 
          sm = ar!r - ar!(l-1)
      print sm
      aquery n qs ar

move :: Int -> Int -> MyArray -> MyArray 
move 0 _ ar = ar
move c n ar = move (c-1) n $ 
  let h = ar!1
      ls = [1..(n-1)]
   in ar // zip ls (map (\i -> ar!(i+1)-h) ls) 

