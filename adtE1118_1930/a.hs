ints :: IO [Int]
ints = map read . words <$> getLine

main :: IO ()
main = do
  [a,b] <- ints
  let res = (a+b)^2 
  print res
