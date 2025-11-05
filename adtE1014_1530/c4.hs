import qualified Data.ByteString.Char8 as B
import Data.List (unfoldr)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Vector.Unboxed as U 
import qualified Data.Vector.Primitive.Mutable as M 

intsb :: IO [Int]
intsb = unfoldr (B.readInt . B.dropSpace) <$> B.getLine

main :: IO ()
main = do
  [_,s] <- intsb
  as <- intsb
  let asv = U.filter (<s) $ U.fromList as
  let mx = U.maximum asv
  ref <- (M.new s) :: IO (M.MVector M.RealWorld Int)
  ans <- if U.null asv || mx*2<s then return (0::Int) else 
              U.foldM' (\an v -> do
                    rfn <- M.read ref (s-v)
                    M.modify ref (+1) v
                    return (an+rfn)) 0 asv
  print ans


