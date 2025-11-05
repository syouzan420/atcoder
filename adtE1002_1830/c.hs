import Control.Monad (replicateM)
import qualified Data.ByteString.Char8 as B
import Data.List (unfoldr)

ints :: IO [Int]
ints = unfoldr (B.readInt . B.dropSpace) <$> B.getLine

main :: IO ()
main = do
 n <- readLn
 wxs <- replicateM n ints
 let (ws,xs) = unzip $ map topair wxs
 let tms = map tms xs
 let res = joinsum wxs 
 putStrLn res


times :: Int -> [Int]
times n = let hd = (9+n) `mod` 24 
           in map (`mod` 24) [9+n..18+n]


topair :: [Int] -> (Int,Int)
topair [w,x] = (w,x)
topair _ = (0,0)
