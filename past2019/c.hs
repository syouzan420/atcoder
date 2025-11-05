import Data.List (sort)

ints :: IO [Int]
ints = map read . words <$> getLine

main :: IO ()
main = do
  lst <- ints
  let slst = sort lst 
  let rslst = reverse slst
  let res = rslst!!2
  print res

