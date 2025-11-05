ints :: IO [Int]
ints = map read . words <$> getLine

main :: IO ()
main = do
  [_,p,q,r,s] <- ints
  as <- ints
  let (top,btm) = splitAt (p-1) as
  let (pq,btm2) = splitAt (q-p+1) btm 
  let (qr,btm3) = splitAt (r-q-1) btm2
  let (rs,btm4) = splitAt (s-r+1) btm3
  let b = top++rs++qr++pq++btm4
  let res = unwords $ map show b 
  putStrLn res
