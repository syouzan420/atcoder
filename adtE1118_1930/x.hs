import qualified Data.ByteString.Char8 as B
import Data.List (unfoldr)

import qualified Data.IntMap as M
import Data.IntMap ((!))

import qualified Data.Array as A 

import qualified Data.Vector.Unboxed as U

import qualified Data.Vector.Algorithms as VA

ints :: IO [Int]
ints = unfoldr (B.readInt . B.dropSpace) <$> B.getLine

