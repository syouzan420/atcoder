{-# LANGUAGE TupleSections #-}

import qualified Data.ByteString.Char8 as B
import Data.List (unfoldr,sortBy,foldl')
import Data.Containers.ListUtils (nubInt)
import qualified Data.IntMap as M
import Data.Ord (Down(..),comparing)

ints :: IO [Int]
ints = unfoldr (B.readInt . B.dropSpace) <$> B.getLine

main :: IO ()
main = do
  n <- readLn :: IO Int
  as <- ints
  let tem = M.fromList (zip ((sortBy (comparing Down) . nubInt) as) [(0::Int)..])
  let ref = M.fromList (map (,0::Int) [0..(n-1)]) 
  let anv = foldl' (\rf e-> let len = tem M.! e 
                             in M.update (\v->Just (v+1)) len rf 
                   ) ref as
  putStr $ unlines  $ map show (M.elems anv) 

