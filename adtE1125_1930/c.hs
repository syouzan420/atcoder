main :: IO ()
main = do
  [n,k] <- map read . words <$> getLine :: IO [Int]
  s <- getLine
  let os = getOs 0 s
  let res = sum $ map (`div` k) os
  print res

getOs :: Int -> String -> [Int]
getOs acc [] = [acc]
getOs acc (x:xs) = if x=='O' then getOs (acc+1) xs 
                             else acc:getOs 0 xs

