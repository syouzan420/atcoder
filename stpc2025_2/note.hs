import qualified Data.ByteString.Char8 as B
import Data.List (unfoldr)
import qualified Data.Vector.Unboxed as U
import Data.Vector.Algorithms (nub)

ints :: IO [Int]
ints = unfoldr (B.readInt . B.dropSpace) <$> B.getLine

main :: IO ()
main = do
  n <- readLn :: IO Int
  as <- ints
  let asu = U.fromList as
  let ref = U.replicate n (0::Int)
  let anv = U.foldl' (\rf e-> let len = U.length $ nub $ U.filter (>e) asu
                               in U.accum (+) rf [(len,1)]
                     ) ref asu
  putStr (unlines (map show (U.toList anv))) 

