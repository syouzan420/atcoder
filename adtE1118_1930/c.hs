main :: IO ()
main = do
  s <- getLine 
  t <- getLine
  let ib = check s t
  let res = if ib then "Yes" else "No" 
  putStrLn res

check :: String -> String -> Bool
check [] _ = True
check (x:xs) (y:ys) 
  | x==y = check xs ys
  | null xs = False
  | head xs == y && head ys == x = True
  | otherwise = False
                              
