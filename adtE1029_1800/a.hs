
ints :: IO [Int]
ints = map read . words <$> getLine

main :: IO ()
main = do
  [_,x,y,z] <- ints
  let b
        | y>x = x<=z && z<=y
        | otherwise = y<=z && z<=x
  let res = if b then "Yes" else "No" 
  putStrLn res
