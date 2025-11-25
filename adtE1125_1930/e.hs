import Data.List (sort)

ints :: IO [Int]
ints = map read . words <$> getLine

main :: IO ()
main = do
  [_,k] <- ints
  rs <- ints
  let lsts = sort $ filter (\ls -> sum ls `mod` k == 0) $ mapM (\r -> [1..r]) rs
  putStr $ unlines $ map (unwords . map show) lsts
  
          
    

