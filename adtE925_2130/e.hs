import Data.Functor ((<&>))

ints :: IO [Int]
ints = getLine <&> map read . words

main :: IO ()
main = do
  _ <- getLine 
  ss <- ints
  ts <- ints
  let h = head ts
  let as = h:addh h ss
  let res = zipWith min as ts
  mapM_ print res  

addh :: Int -> [Int] -> [Int]
addh _ [] = []
addh i (x:xs) = i+x:addh (i+x) xs
