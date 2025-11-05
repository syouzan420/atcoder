import qualified Data.ByteString.Char8 as B
import Data.List (unfoldr)
import Data.Array.IArray (listArray,(!))
import Data.Array.Unboxed (UArray)

intsb :: IO [Int]
intsb = unfoldr (B.readInt . B.dropSpace) <$> B.getLine

main :: IO ()
main = do
  [n,s] <- intsb
  as <- intsb
  let asa = listArray @UArray (1,n) as
  let res = length [ True |x<-[(1::Int)..(n-1)],y<-[x+1..n],asa!x+asa!y==s] 
  print res


