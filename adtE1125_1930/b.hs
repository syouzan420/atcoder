main :: IO ()
main = do
  x <- readLn :: IO Float 
  let res i 
        | i>=38.0 = 1 :: Int
        | i>=37.5 = 2
        | otherwise = 3
  print (res x)
