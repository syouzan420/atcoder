import qualified Data.ByteString.Char8 as B
import Data.List (unfoldr)
import qualified Data.Vector.Unboxed.Mutable as M
import qualified Data.Vector.Unboxed as U
import qualified Data.Massiv.Array as A
import Data.Vector.Algorithms (nub)
import Data.Vector.Algorithms.Heap (sort)
import Data.Vector.Generic (group)

type MV = M.MVector M.RealWorld Int

rint :: IO Int
rint = readLn

ints :: IO [Int]
ints = unfoldr (B.readInt . B.dropSpace) <$> B.getLine

main :: IO ()
main = do
  n <- rint
  as <- ints
  let asu = U.fromList as
  asv <- U.thaw asu
  sort asv
  ref <- M.replicate n 0 >>= getbig n asv >>= U.freeze
  let gref = reverse $ map U.length $ U.group ref
  let len = length gref
  let ans = map show gref ++ replicate (n-len) "0" 
  putStr (unlines ans) 


getbig :: Int -> MV -> MV -> IO MV 
getbig 0 _ r = return r 
getbig i v r = do 
  h <- M.read v (i-1)
  let t = M.drop i v
  if M.null t then M.write r (i-1) 0 else do
    t' <- U.freeze t
    let nb = nub t'
    let lng = U.length nb
    let hnb = (U.!) nb 0
    let nm = if hnb==h then lng-1 else lng
    M.write r (i-1) nm
  getbig (i-1) v r

