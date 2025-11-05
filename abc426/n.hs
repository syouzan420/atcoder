import Control.Monad (replicateM)
import qualified Data.ByteString.Char8 as B
import Data.List (unfoldr)

import Data.Array (Array,listArray,(!),(//))

import qualified Data.IntMap as M

import qualified Data.Vector as V

intsb :: IO [Int]
intsb = unfoldr (B.readInt . B.dropSpace) <$> B.getLine

ints :: IO [Int]
ints = map read . words <$> getLine
