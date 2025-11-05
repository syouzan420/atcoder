import qualified Data.IntMap as M
import Data.IntMap ((!))
import qualified Data.ByteString.Char8 as B
import Data.List (unfoldr,scanl')

ints :: IO [Int]
ints = unfoldr (B.readInt . B.dropSpace) <$> B.getLine

main :: IO ()
main = do
  [n,a,b] <- ints
  s <- getLine
  let mpa = M.fromList (zip [0..n] (scanl' (\i ch->if ch=='a' then i+1 else i) 0 s))
  let mpb = M.fromList (zip [0..n] (scanl' (\i ch->if ch=='b' then i+1 else i) 0 s))
  let res = check n a b mpa mpb 1 
  print res

check :: Int -> Int -> Int -> M.IntMap Int -> M.IntMap Int -> Int -> Int
check n a b mpa mpb l
    | l>n = 0 
    | otherwise = let mia = mpa!(l-1)
                      mib = mpb!(l-1)
                      r = (n+l) `div` 2
                      ra = if mia+a>mpa!n then 0 else getra l r n (mia+a) mpa 
                      rb = getrb l r n (mib+b) mpb
                      rn = if ra/=0 && rb>=ra then rb-ra+1 else 0
                   in rn+check n a b mpa mpb (l+1)

getra :: Int -> Int -> Int -> Int -> M.IntMap Int -> Int
getra l r n al mpa 
    | mpa!r >= al = if r==l then l 
                            else let nr = (r+l) `div` 2 in getra l nr r al mpa 
    | otherwise = if r==l then n
                          else let nr = (n+r) `div` 2 in getra r nr n al mpa

getrb :: Int -> Int -> Int -> Int -> M.IntMap Int -> Int
getrb l r n bl mpb 
    | r==l = l
    | mpb!r < bl = let nr = (n+r) `div` 2 in getrb r nr n bl mpb 
    | otherwise =  let nr = (r+l) `div` 2 in getrb l nr r bl mpb
