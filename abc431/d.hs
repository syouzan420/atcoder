import Control.Monad (replicateM)
import Data.List (scanl',sort,transpose)
import qualified Data.IntMap as M
import Data.IntMap ((!))
import qualified Data.ByteString.Char8 as B
import Data.List (unfoldr)


ints :: IO [Int]
ints = unfoldr (B.readInt . B.dropSpace) <$> B.getLine

main :: IO ()
main = do
  n <- readLn :: IO Int
  whbs <- replicateM n ints 
  let res = 0 
  print res 
