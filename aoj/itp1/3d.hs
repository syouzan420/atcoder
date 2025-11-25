main = do
  [a,b,c] <- map read . words <$> getLine :: IO [Int]
  print $ yaku a b c

yaku :: Int -> Int -> Int -> Int
yaku i b c
  | i>b = 0
  | otherwise = if c `mod` i == 0 then 1+nyaku else nyaku
  where nyaku = yaku (i+1) b c
