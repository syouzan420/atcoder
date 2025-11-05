main :: IO ()
main = do
  [x,y] <- getLine >>= return . map read . words
  let mt = cycle [1..12]
  let ls = take (x+y) mt
  let res = last ls
  print res      

