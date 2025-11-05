main :: IO ()
main = do
  s <- getLine
  let ch = getcenter s
  putStrLn [ch]

getcenter :: String -> Char
getcenter str = let l = length str
                    ci = l `div` 2
                 in str!!ci
