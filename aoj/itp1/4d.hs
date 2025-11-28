main = do
  _ <- getLine
  as <- map read . words <$> getLine :: IO [Int]
  putStrLn $ unwords $ map (\f->(show.f) as) [minimum,maximum,sum] 
