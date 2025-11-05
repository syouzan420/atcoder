{-# LANGUAGE TupleSections #-}
import qualified Data.ByteString.Char8 as B
import qualified Data.Vector.Unboxed as U
import qualified Data.IntMap as M 
import Data.Maybe (fromMaybe)
import Data.List (unfoldr)

intsb :: IO [Int]
intsb = unfoldr (B.readInt . B.dropSpace) <$> B.getLine

main :: IO ()
main = do
  [_,s] <- intsb
  as <- intsb
  let asv = U.fromList as
  let asm = M.fromList (map (,0::Int) as)
  let (ans,_) = U.foldl' (\(a,rf) k->
                 (a+fromMaybe 0 (M.lookup (s-k) rf)
                 ,M.update (\x->Just (x+1)) k rf) 
                      ) (0,asm) asv
  print ans


