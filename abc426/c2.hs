{-# LANGUAGE TupleSections #-}

import Control.Monad (replicateM)
import qualified Data.ByteString.Char8 as B
import Data.List (unfoldr)
import qualified Data.Map.Strict as M
import Data.Maybe (isJust,isNothing,fromJust)

type MP = M.Map Int Int

intsb :: IO [Int]
intsb = unfoldr (B.readInt . B.dropSpace) <$> B.getLine

main :: IO ()
main = do
  [n,q] <- intsb
  xys <- replicateM q $ intsb >>= (\[x,y] -> return (x,y))
  let m = M.fromList $ map (,1) [1..n] 
  putStr $ unlines $ map show $ solve m xys 

solve :: MP -> [(Int,Int)] -> [Int]
solve _ [] = []
solve m ((x,y):xys)
  | isJust m2 = (cnt1+cnt2) : solve m5 xys
  | isNothing m2 = cnt1 : solve m4 xys
  | otherwise = 0 : solve m xys
  where
    (m1,m2,m3) = M.splitLookup x m
    acs = M.assocs m1 :: [(Int,Int)]
    m4 = foldl (\acc (_,b) -> M.adjust (+b) y acc) m3 acs :: MP
    m5 = M.insertWith (+) y (fromJust m2) m4
    cnt1 = sum $ map snd acs
    cnt2 = fromJust m2

