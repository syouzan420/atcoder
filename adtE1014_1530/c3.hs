import qualified Data.ByteString.Char8 as B
import Data.List (unfoldr)
import qualified Data.Vector.Unboxed as U
import Data.Vector.Unboxed ((!))

intsb :: IO [Int]
intsb = unfoldr (B.readInt . B.dropSpace) <$> B.getLine

main :: IO ()
main = do
  [_,s] <- intsb
  as <- intsb
  let asv = U.filter (<s) $ U.fromList as
  let mx = U.maximum asv
  let ref = U.replicate s (0::Int)
  let (ans,_) = if U.null asv || mx*2<s then (0,ref) else 
       U.foldl' (\(an,rf) v -> (an+(rf!(s-v)),U.accum (+) rf [(v,1)])) (0,ref) asv
  print ans 


