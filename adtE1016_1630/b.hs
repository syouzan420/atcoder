
ints :: IO [Int]
ints = map read . words <$> getLine

main :: IO ()
main = do
  [n,d] <- ints
  ts <- ints
  let x2 = check d ts 
  print x2 

check :: Int -> [Int] -> Int
check _ [] = -1
check _ [_] = -1
check d (x1:x2:xs) = if x2-x1<=d then x2 else check d (x2:xs)

