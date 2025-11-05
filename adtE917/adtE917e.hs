import Data.Functor ((<&>))
import Control.Monad (replicateM)
import Data.Array (Array,listArray,elems,(//),(!))
import Data.List (transpose)

type MA = Array (Int,Int) Int
type ANA = Array Int (Int,Int)

main :: IO ()
main = do
  [h,w,n] <- getLine <&> map read . words
  abs <- replicateM n (getLine <&> map (\i->read i::Int) . words)
  let ia = listArray ((1,1),(h,w)) (repeat 0)
  let ansa = listArray (1,n) (replicate n (0,0))
  let (_,ua) = foldl updateA (1,ia) abs
  let ul = chunkOf w $ elems ua 
  let ulh = filter (not . all (==0)) ul
  let trl = transpose ulh 
  let tulhw = filter (not . all (==0)) trl
  let finl = transpose tulhw 
  let (_,ansau) = foldl scanNum ((1,1),ansa) finl 
  let res = elems ansau
  mapM_ (\(a,b) -> putStrLn (show a ++ " " ++ show b)) res

scanNum :: ((Int,Int),ANA) -> [Int] -> ((Int,Int),ANA)
scanNum ((i,j),ar) [] = ((i+1,1),ar)  
scanNum ((i,j),ar) (w:ws) = if w==0 then scanNum ((i,j+1),ar) ws
                                     else scanNum ((i,j+1),ar//[(w,(i,j))]) ws

chunkOf :: Int -> [a] -> [[a]]
chunkOf n ls
  | length ls <= n = [ls]
  | otherwise = take n ls : chunkOf n (drop n ls)

updateA :: (Int,MA) -> [Int] -> (Int,MA) 
updateA (i,ar) [a,b] = (i+1, ar // [((a,b),i)])
updateA (_,ar) _ = (0,ar)

