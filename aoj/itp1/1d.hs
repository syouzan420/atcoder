main = do
  i <- readLn :: IO Int
  let mi = i `div` 60
  let s = i `mod` 60
  let h = mi `div` 60
  let m = mi `mod` 60
  putStrLn $ show h ++ ":" ++ show m ++ ":" ++ show s
