import qualified Data.ByteString.Char8 as B
import Data.List (unfoldr)

ints :: IO [Int]
ints = unfoldr (B.readInt . B.dropSpace) <$> B.getLine

