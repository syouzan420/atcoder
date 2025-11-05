import Control.Monad (replicateM)
import qualified Data.ByteString.Char8 as B
import Data.List (unfoldr)
import qualified Data.IntMap as M

type IM = M.IntMap Int

intsb :: IO [Int]
intsb = unfoldr (B.readInt . B.dropSpace) <$> B.getLine

main :: IO ()
main = do
  [n,q] <- intsb
  xys <- replicateM q intsb
  let im = M.fromList (zip [1..n] [1..n])
  let (_,res) = foldl upg (im,[]) xys
  mapM_ print res

upg :: (IM,[Int]) -> [Int] -> (IM,[Int])
upg (vs,nm) [x,y] =
   let tgs = M.filter (<=x) vs
       n = M.size tgs
       ntgs = M.map (const y) tgs
       nvs = M.union ntgs vs
    in (nvs,nm++[n])
upg _ _ = (M.fromList [],[])
