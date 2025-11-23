import Data.List (group,isPrefixOf)

main :: IO ()
main = do
  s <- getLine
  let gs = group s
  let res = check gs
  print res 

check :: [String] -> Int
check [] = 0
check [_] = 0
check (x:y:xs) = let x' = concatMap (show . (\i -> read [i] + 1 ::Int)) x
                     rs 
                      | head x == '9' = 0
                      | x' `isPrefixOf` y = length x
                      | y `isPrefixOf` x' = length y 
                      | otherwise = 0
                  in rs+check (y:xs)
