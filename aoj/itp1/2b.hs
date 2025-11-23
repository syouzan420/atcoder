main = (map read . words <$> getLine :: IO [Int]) 
          >>= \[a,b,c] -> putStrLn $ if a < b && b < c then "Yes" else "No"
