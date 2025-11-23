main = do
  [w,h,x,y,r] <- map read . words <$> getLine :: IO [Int]
  let ib = x-r >= 0 && x+r <= w && y-r >= 0 && y+r <= h
  putStrLn $ if ib then "Yes" else "No"
