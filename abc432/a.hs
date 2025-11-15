import Data.List (sort)

ints :: IO [Int]
ints = map read . words <$> getLine

main :: IO ()
main = do
  abc <- ints
  let res = (concatMap show . reverse . sort) abc 
  putStrLn res
