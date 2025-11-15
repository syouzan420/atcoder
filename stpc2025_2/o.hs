import qualified Data.ByteString.Char8 as B
import Data.List (unfoldr)
import qualified Data.IntMap as M
import Data.Maybe (fromMaybe)

ints :: IO [Int]
ints = unfoldr (B.readInt . B.dropSpace) <$> B.getLine

main :: IO ()
main = do
  [n,l,w] <- ints
  ds <- ints
  let mp = M.fromList (map (,0) ds)
  let hi = headi 1 l w 
  let x = solve hi w mp 
  print x

headi :: Int -> Int -> Int -> Int
headi i l w = if i*w >= l then i else headi (i+1) l w

solve :: Int -> Int -> M.IntMap Int -> Int
solve i w ds = let x = i*w
                   k = fromMaybe x (M.lookup x ds)
                in if k==0 then solve (i+1) w ds else k

