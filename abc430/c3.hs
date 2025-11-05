import qualified Data.ByteString.Char8 as B
import Data.List (unfoldr,scanl')
import qualified Data.Vector.Unboxed as U
import Data.Vector.Unboxed ((!))

ints :: IO [Int]
ints = unfoldr (B.readInt . B.dropSpace) <$> B.getLine

main :: IO ()
main = do
  [n,a,b] <- ints
  s <- getLine
  let vs = U.fromList s
  let ara = U.scanl' (\i ch->if ch=='a' then i+1 else i) 0 vs
  let arb = U.scanl' (\i ch->if ch=='b' then i+1 else i) 0 vs
  let res = check n a b ara arb (1,a) 
  print res

check :: Int -> Int -> Int -> U.Vector Int -> U.Vector Int -> (Int,Int) -> Int 
check n a b ara arb (l,r)
    | l>n = 0
    | r>n = ncheck (l+1,l+a)
    | otherwise = let ia = (ara!r - ara!(l-1)) >= a
                      ib = (arb!r - arb!(l-1)) < b
                   in if ia && ib then 1+ncheck (l,r+1)  
                                  else if not ib then ncheck (l+1,l+a) 
                                                 else ncheck (l,r+1)
    where ncheck = check n a b ara arb
