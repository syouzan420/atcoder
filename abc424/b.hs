import Data.Functor ((<&>))
import Control.Monad (replicateM)
import Data.List (intersperse)

main :: IO ()
main = do
  [_,m,k] <- getLine <&> map read . words 
  ab <- replicateM k $ getLine <&> map read . words
  let (pss,_) = foldl (correct m) ([],[]) ab
  let res = intersperse ' ' $ concatMap show pss
  if null pss then return () else putStrLn res

correct :: Int -> ([Int],[Int]) -> [Int] -> ([Int],[Int])
correct n (ps,ms) [a,_] = let nms = a:ms 
                              fms = filter (==a) nms
                              nps = if length fms == n then ps++[a] else ps 
                           in (nps,nms) 
correct _ _ _ = ([],[]) 


