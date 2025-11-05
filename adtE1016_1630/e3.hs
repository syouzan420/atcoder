{-# LANGUAGE TupleSections #-}

import qualified Data.ByteString.Char8 as B
import Data.List (unfoldr)
import qualified Data.Vector.Unboxed as U
import qualified Data.IntMap as M
import Data.Vector.Algorithms (nub)

ints :: IO [Int]
ints = unfoldr (B.readInt . B.dropSpace) <$> B.getLine

main :: IO ()
main = do
  n <- readLn :: IO Int
  as <- ints
  let asu = U.fromList as
  let ref = M.fromList (map (,0) [0..(n-1)]) 
  let anv = U.foldl' (\rf e-> let len = U.length $ nub $ U.filter (>e) asu
                               in M.update (\v->Just (v+1)) len rf
                     ) ref asu
  putStr (unlines (map show (M.elems anv))) 

