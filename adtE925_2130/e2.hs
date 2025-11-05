import Data.Functor ((<&>))
import Data.Graph.Inductive.Graph (mkGraph)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.Query.SP (spLength)
import Data.Maybe (fromMaybe)

ints :: IO [Int]
ints = getLine <&> map read . words

main :: IO ()
main = do
  n <- readLn 
  ss <- ints
  ts <- ints
  let gr = genGraph n ss ts
  let res = map (\i -> fromMaybe 0 $ spLength 0 i gr) [1..n]
  mapM_ print res  

genGraph :: Int -> [Int] -> [Int] -> Gr Int Int
genGraph n ss ts = mkGraph nodes edges
  where
    nodes = map (\i -> (i,i)) [0..n]
    edges = let z = zipWith (\i d -> (0,i,d)) [1..n] ts
                ot = zipWith (\i d -> (i,i+1,d)) [1..(n-1)] (init ss)
                ls = [(n,1,last ss)]
             in z++ot++ls   
