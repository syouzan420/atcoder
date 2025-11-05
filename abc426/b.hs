
main :: IO ()
main = do
  s <- getLine
  let res=check s ' '
  putStrLn [res]

check :: String -> Char -> Char
check [] _ = ' '
check (x:xs) p
  | p==' ' = check xs x
  | x==p = check xs p
  | otherwise = if null xs then x else let h=head xs 
                                        in if x==h then p else x  

