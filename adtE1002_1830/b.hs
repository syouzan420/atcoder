
getint :: IO Int
getint = readLn

main :: IO ()
main = do
  n <- getint 
  let ib = iscon n 
  let res = if ib then "Yes" else "No"
  putStrLn res

iscon :: Int -> Bool
iscon n = let n'=fromIntegral n in log 2 > 2*log n'/n' 

