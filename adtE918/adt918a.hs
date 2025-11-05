
main :: IO ()
main = do
  _ <- getLine 
  s <- getLine
  let b = check s
  let res = if b then "Yes" else "No"
  putStrLn res

check :: String -> Bool
check t = let lt = length t 
              isOdd = odd $ lt
              (f,s) = break (=='/') t
              tind = (lt+1) `div` 2
              isOne = length f == tind - 1 && isAll '1' f 
              twos = tail s
              isTwo = isAll '2' twos
           in isOdd && isOne && isTwo 

isAll :: Char -> String -> Bool
isAll ch str = str == filter (==ch) str
