import Data.List (nub)

main :: IO ()
main = do
  _ <- readLn :: IO Int
  as <- map read . words <$> getLine :: IO [Int]
  let ib = length (nub as) == 1
  let res = if ib then "Yes" else "No" 
  putStrLn res
