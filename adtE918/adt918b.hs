
main :: IO ()
main = do
  n <- readLn
  let res = qnum n
  print res

qnum :: Int -> Int
qnum n
  | n < 126 = 4 
  | n < 212 = 6
  | otherwise = 8

