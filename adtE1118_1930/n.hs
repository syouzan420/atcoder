import Data.List (sort,group)

ints :: IO [Int]
ints = map read . words <$> getLine

main :: IO ()
main = do
  as <- ints
  let lns = (map length . group . sort) as 
  let ib = lns == [3,2] || lns == [2,3] 
  let res = if ib then "Yes" else "No" 
  putStrLn res

