import Control.Monad (replicateM_)
import qualified Data.ByteString.Char8 as B
import Data.List (unfoldr)

ints :: IO [Int]
ints = unfoldr (B.readInt . B.dropSpace) <$> B.getLine

main :: IO ()
main = do
  t <- readLn :: IO Int
  replicateM_ t tcase

tcase :: IO ()
tcase = do
  [a,b,c,d] <- ints 
  let bl = check a b c d
  let res = if bl then "Yes" else "No"
  putStrLn res

check :: Int -> Int -> Int -> Int -> Bool
check a b c d = let a' = fromIntegral a
                    b' = fromIntegral b
                    c' = fromIntegral c
                    d' = fromIntegral d
                    bls = map test (qumi [a',b',c',d'])
                 in or bls

qumi :: [Double] -> [[Double]]
qumi [a,b,c,d] = [[a,b,c,d],[b,c,d,a],[c,d,a,b],[d,a,b,c]]

test :: [Double] -> Bool
test [a,b,c,d] = a+b > c+d && abs (b-a) < c+d
