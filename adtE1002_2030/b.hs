
ints :: IO [Int]
ints = map read . words <$> getLine

main :: IO ()
main = do
  [a,b,d] <- ints
  let res = getres a b d 
  putStrLn (unwords (map show res))

getres :: Int -> Int -> Int -> [Int]
getres a b d = [a,(a+d)..b]
