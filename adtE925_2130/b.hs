main :: IO ()
main = do
  d <- readLn
  let p = fromIntegral d / 100
  print p

