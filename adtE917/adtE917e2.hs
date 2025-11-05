import Data.Functor ((<&>))
import Control.Monad (replicateM)
import Data.Bifunctor (bimap)


main :: IO ()
main = do
  [h,w,n] <- getLine <&> map read . words
  abs <- replicateM n (getLine <&> map (\i->read i::Int) . words)
  let (as,bs) = toPair abs 
  let (asu,bsu) = (numbering 1 as, numbering 1 bs)
  mapM_ (\(a,b) -> putStrLn (show a ++ " " ++ show b)) (zip asu bsu)

toPair :: [[Int]] -> ([Int],[Int])
toPair [] = ([],[])
toPair ([a,b]:xs) = let next = toPair xs in bimap (a:) (b:) next
toPair (_:_) = ([],[])

numbering :: Int -> [Int] -> [Int]
numbering i ls = let fls = filter (>=i) ls
                  in if null fls then ls 
                                 else numbering (i+1) $ change (minimum fls) i ls

change :: Int -> Int -> [Int] -> [Int]
change _ _ [] = []
change m i (x:xs) = if x==m then i:change m i xs else x:change m i xs

