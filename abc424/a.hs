import Data.Functor ((<&>))
import Data.List (sort)

main :: IO ()
main = do
  abc <- getLine <&> map read . words
  let b = check abc
  let res = if b then "Yes" else "No"
  putStrLn res


check :: [Int] -> Bool
check abc = let [a,b,c] = sort abc 
             in (a==b || b==c) && a+b>c
                
