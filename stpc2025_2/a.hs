import Control.Monad (replicateM_)

ints :: IO [Int]
ints = map read . words <$> getLine


main :: IO ()
main = do
  q <- readLn :: IO Int
  replicateM_ q tcase 

tcase :: IO ()
tcase = do
  [n,k] <- ints
  let res = test n k 
  putStrLn $ unlines res

test :: Int -> Int -> [String]
test n k = undefined

