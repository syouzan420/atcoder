main :: IO ()
main = do
  n <- readLn :: IO Int
  as <- map read . words <$> getLine :: IO [Int]
  let res = reverse $ check (reverse (zip [1..] as)) 
  putStr $ unlines $ map show res

check :: [(Int,Int)] -> [Int] 
check [] = []
check ((_,a):as) = let hs = filter (\(_,b)-> b>a) as
                       i = if null hs then -1 else (fst . head) hs 
                    in i:check as
