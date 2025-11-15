import qualified Data.ByteString.Char8 as B
import Data.List (unfoldr)

import qualified Data.IntMap as M
import Data.IntMap ((!))


ints :: IO [Int]
ints = unfoldr (B.readInt . B.dropSpace) <$> B.getLine


main :: IO ()
main = do
  [n,x,y] <- ints
  (a1:as) <- ints
  let ch1 = M.fromList [(x*sm+y*(a1-sm),fromIntegral (a1-sm)) | sm<-[0..a1]] 
  let ngms = check x y ch1 as
  let bgs = if null ngms then (-1) else last (M.elems ngms)
  print bgs

check :: Int -> Int -> M.IntMap Integer -> [Int] -> M.IntMap Integer
check _ _ gms [] = gms 
check x y ch1 (a:as) = 
  let gms = M.fromList [let gm=x*sm+y*(a-sm) in (gm,ch1!gm+fromIntegral (a-sm)) | sm<- [0..a], let gm = x*sm+y*(a-sm) in M.member gm ch1]
   in if null gms then M.empty else check x y gms as

