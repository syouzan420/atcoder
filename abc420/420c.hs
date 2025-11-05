import Control.Monad (replicateM,foldM)
import Data.Array (listArray,elems,Array,(!),(//))

getInts :: Int -> IO [[Int]]
getInts n = replicateM n $ getLine >>= return . map read . words

type MyArray = Array Int Int

main :: IO ()
main = do
  [[n,q]]  <- getInts 1
  [as,bs]  <- getInts 2
  let ara = listArray (1,n) as 
  let arb = listArray (1,n) bs 
  cxvs <- replicateM q $ getLine >>= return . words 
  let (_,_,nls) = foldl newab (ara,arb,[]) cxvs
  mapM_ (putStrLn . show) nls

newab :: (MyArray,MyArray,[Int]) -> [String] -> (MyArray,MyArray,[Int])
newab (ara,arb,ls) (ch:xs) = 
  let [x,v]::[Int] = map read xs
      (ara',arb') = if ch=="A" then (ara//[(x,v)],arb) else (ara,arb//[(x,v)])  
      as = elems ara'
      bs = elems arb'
      sm = sum $ zipWith min as bs
   in (ara',arb',ls++[sm])
  

