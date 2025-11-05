rint :: IO Int
rint = readLn

main :: IO ()
main = do
  n <- rint
  let ans = getnum n 
  print ans 

getnum :: Int -> Int
getnum 1000 = 0
getnum x = let [a,b,c] = map (\i -> read [i] :: Int) $ show x
            in if a*b==c then x else getnum (x+1)



