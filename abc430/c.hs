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
  let mpa = M.fromList (zip [0..(n+1)] (scanl' (\i ch->if ch=='a' then i+1 else i) 0 (s++"b")))
  let mpb = M.fromList (zip [0..(n+1)] (scanl' (\i ch->if ch=='b' then i+1 else i) 0 (s++"b")))
  let res = check 1 (n+1) a b mpa mpb 
  print res

check :: Int -> Int -> Int -> Int -> M.IntMap Int -> M.IntMap Int -> Int
check l r a b mpa mpb
    | l>r = 0 
    | otherwise = let mia = mpa!(l-1)
                      mib = mpb!(l-1)
                      m = (l+r) `div` 2
                      ma = if mia+a>mpa!r then 0 else getra l m r (mia+a) mpa 
                      mb = getrb l m r (mib+b) mpb
                      mn = if ma/=0 && mb>=ma then mb-ma+1 else 0
                   in mn+check (l+1) r a b mpa mpb

getra :: Int -> Int -> Int -> Int -> M.IntMap Int -> Int
getra l m r al mpa 
    | mpa!m >= al = if m==l then l 
                            else let nm = (l+m) `div` 2 in getra l nm m al mpa 
    | otherwise = if m==l then r
                          else let nm = (m+r) `div` 2 in getra m nm r al mpa

getrb :: Int -> Int -> Int -> Int -> M.IntMap Int -> Int
getrb l m r bl mpb 
    | m==l = l
    | mpb!m < bl = let nm = (r+m) `div` 2 in getrb m nm r bl mpb 
    | otherwise =  let nm = (m+l) `div` 2 in getrb l nm m bl mpb
