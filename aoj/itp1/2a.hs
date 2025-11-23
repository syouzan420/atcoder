main = (map read . words <$> getLine :: IO [Int])
         >>= \[a,b]-> putStrLn $ 
          case compare a b of LT -> "a < b"; GT -> "a > b"; EQ -> "a == b" 
