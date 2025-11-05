import Data.Functor ((<&>))

main :: IO ()
main = do
  [x,k] <- getLine <&> map (\i -> read i::Integer) . words
  let res = ""
  putStrLn res

myround :: Integer -> Integer -> Integer
myround x i = undefined

