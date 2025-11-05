import Data.Functor ((<&>))

main :: IO ()
main = do
  _ <- getLine 
  ps <- getLine <&> map read . words
  let zyun = getZun 1 (replicate (length ps) 0) ps ps
  mapM_ print zyun 

getZun :: Int -> [Int] -> [Int] -> [Int] -> [Int]
getZun _ ac _ [] = ac 
getZun r ac ops ps = 
  let mps = maximum ps
      tps = filter (==mps) ps
      lt = length tps
      nps = filter (/=mps) ps
      nls = map (\i -> if i==mps then r else 0) ops
      nac = zipWith (\a p -> if p==r then r else a) ac nls
   in getZun (r+lt) nac ops nps 
  
