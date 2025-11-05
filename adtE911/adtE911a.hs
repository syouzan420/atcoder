main :: IO ()
main = do
  s <- getLine
  let res = map (\ch -> if ch=='0' then '1' else '0') s
  putStrLn res
