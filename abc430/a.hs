
ints :: IO [Int]
ints = map read . words <$> getLine

main :: IO ()
main = do
  [a,b,c,d] <- ints
  let bl = (c>=a && d>=b) || c<a
  let res = if bl then "No" else "Yes" 
  putStrLn res
