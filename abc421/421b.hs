{-# LANGUAGE ScopedTypeVariables #-}

main :: IO ()
main = do
  n :: Int <- getLine >>= return . read
  s <- getLine
  let res = fib x y 10
  print res

fib :: Int -> Int -> Int -> Int
fib x _ 1 = x
fib _ y 2 = y
fib x y n = f $ fib x y (n-2) + fib x y (n-1)

f :: Int -> Int
f x = let sx = show x
          revSx = reverse sx
       in read $ elimZero revSx

elimZero :: String -> String
elimZero [] = []
elimZero (x:xs) = if x=='0' then elimZero xs else (x:xs)
