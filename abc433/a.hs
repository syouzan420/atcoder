main :: IO ()
main = do
  [x,y,z] <- map read . words <$> getLine :: IO [Int]
  let ib = check x y z
  let res = if ib then "Yes" else "No" 
  putStrLn res

check :: Int -> Int -> Int -> Bool
check x y z 
  | x < y*z = False
  | otherwise = x == y*z || check (x+1) (y+1) z
