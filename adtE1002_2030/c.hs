import Control.Monad (replicateM)

getint :: IO Int
getint = readLn

main :: IO ()
main = do
  n <- getint
  ss <- replicateM n getLine
  let ib = check n ss 
  let res = if ib then "Yes" else "No" 
  putStrLn res

check :: Int -> [String] -> Bool
check 0 _ = False
check _ [] = False
check i str@(h:tl) = let checkone = isrot h tl 
                      in checkone || check (i-1) (shift str) 

shift :: [String] -> [String]
shift str = tail str ++ [head str]

isrot :: String -> [String] -> Bool
isrot s0 [] = False
isrot s0 (x:xs)
  | head s0 /= last x = isrot s0 xs
  | otherwise = let str = s0++x 
                 in str==reverse str || isrot s0 xs

