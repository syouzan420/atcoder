ints :: IO [Int]
ints = map read . words <$> getLine

main :: IO ()
main = do
  [h,b] <- ints
  let res = if h>b then h-b else 0 
  print res
