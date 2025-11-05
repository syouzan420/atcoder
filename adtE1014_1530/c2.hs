import qualified Data.ByteString.Char8 as B
import Data.List (unfoldr)
import qualified Data.Vector.Unboxed as U

intsb :: IO [Int]
intsb = unfoldr (B.readInt . B.dropSpace) <$> B.getLine

main :: IO ()
main = do
  [_,s] <- intsb
  as <- intsb
  let asv = U.fromList as
  let res = 
       U.ifoldl' (\a i x -> 
          a + U.foldl' (\ac y ->
               if x+y==s then ac+1 else ac) 0 (U.drop (i+1) asv)) 0 (U.init asv)
  print res


