
ints :: IO [Int]
ints = map read . words <$> getLine

main :: IO ()
main = do
  [n,m] <- ints
  as <- ints
  let b = m >= sum as
  let res = if b then "Yes" else "No"
  putStrLn res

