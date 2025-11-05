import Control.Monad (replicateM)

main :: IO ()
main = do
  n <- readLn :: IO Int
  as <- replicateM n (readLn :: IO Int)
  let res = check as 
  putStr $ unlines res

check :: [Int] -> [String]
check [] = []
check [_] = []
check (a:b:xs)
        | a==b = "stay":next
        | b<a = ("down "++show (a-b)):next
        | otherwise = ("up "++show (b-a)):next
        where next = check (b:xs)
