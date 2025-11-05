import Control.Monad (replicateM_)
import qualified Data.ByteString.Char8 as B
import Data.List (unfoldr)
import qualified Data.Map.Strict as M

ints :: IO [Int]
ints = map read . words <$> getLine

intsb :: IO [Int]
intsb = unfoldr (B.readInt . B.dropSpace) <$> B.getLine

main :: IO ()
main = do
  let res = ""
  putStrLn res
