import qualified Data.IntMap as M
import Data.IntMap ((!))
import Data.List (scanl')

ints :: IO [Int]
ints = map read . words <$> getLine

main :: IO ()
main = do
  [n,a,b] <- ints
  s <- getLine
  let mpa = M.fromList (zip [0..n] (scanl' (\i ch->if ch=='a' then i+1 else i) 0 s))
  let mpb = M.fromList (zip [0..n] (scanl' (\i ch->if ch=='b' then i+1 else i) 0 s))
  let lr = [(l,r)|l<-[1..n],r<-[l..n]]
  let res = length $ filter (check a b mpa mpb) lr
  print res

check :: Int -> Int -> M.IntMap Int -> M.IntMap Int -> (Int,Int) -> Bool
check a b mpa mpb (l,r) = (mpa!r - mpa!(l-1) >= a) && (mpb!r - mpb!(l-1) < b)
