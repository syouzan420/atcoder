main :: IO ()
main = do
  n <- readLn
  let res = sum $ map calc [(1::Int)..n]
  print res

calc :: Int -> Int
calc i = (-1)^i*(i^3)
