import System.IO (hFlush,stdout)
import System.Random (randomRIO)
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)


main :: IO ()
main = do
  [n,m,l,u] <- getLine >>= return . map read . words
  let aa = avlA n m l u
  let as = genA2' n n aa 
  let aStrs = unwords $ map show as
  putStrLn aStrs 
  hFlush stdout
  bs <- getLine >>= return . map (\i -> read i::Integer) . words
  let xs = genX n m
  let xStrs = unwords $ map show xs
  putStrLn xStrs
  hFlush stdout

genA3 :: Integer -> Integer -> Integer -> Integer -> [Integer]
genA3 0 _ _ _  = []
genA3 i u ac dt = ac + dt*i : genA3 (i-1) u ac dt

genA2' :: Integer -> Integer -> Integer -> [Integer]
genA2' 0 _ _ = []
genA2' i n aa = i - (n `div` 2) + aa : genA2' (i-1) n aa

genA2 :: Integer -> Integer -> Integer -> [Integer]
genA2 0 _ _ = []
genA2 i u aa = i `mod` u + aa : genA2 (i-1) u aa

genA :: Integer -> Integer -> [Integer]
genA 0 _ = []
genA i u = i `mod` u + 1:genA (i-1) u

genAIO2 :: Integer -> Integer -> IO [Integer]
genAIO2 0 _ = return []
genAIO2 i aa = do 
  a <- randomRIO (1,aa)
  as <- genAIO2 (i-1) aa 
  return (a:as)

genAIO :: Integer -> Integer -> IO [Integer]
genAIO 0 _ = return []
genAIO i u = do 
  a <- randomRIO (1,u)
  as <- genAIO (i-1) u
  return (a:as)

avlA :: Integer -> Integer -> Integer -> Integer -> Integer
avlA n m l u =
  let avdiv = n `div` m  -- だいたいn がm の何倍くらゐなのか
      avlu = (l+u) `div` 2  -- l と u の平均
   in avlu `div` avdiv  -- aのひとつがこのくらゐなら avlu個合はせてbひとつになりさう 

dlta :: Integer -> Integer -> Integer -> Integer -> Integer
dlta n m l u = 
  let avdiv = n `div` m
   in (u-l) `div` avdiv

nearest :: Integer -> [Integer] -> Integer
nearest a bs = let def = map (abs .(+) (-a)) bs
                   defmin = minimum def
                   ind = fromMaybe 0 (elemIndex defmin def)
                in fromIntegral ind

changeBs :: Integer -> Integer -> [Integer] -> [Integer]
changeBs a i bs =
  let ind = fromIntegral i 
      b = bs !! ind
      nb = b - a
      nb' = max nb 0
   in take ind bs ++ [nb'] ++ drop (ind+1) bs

genX2 :: [Integer] -> [Integer] -> [Integer]
genX2 _ [] = []
genX2 bs (a:as) = let ni = nearest a bs
                      ni' = if bs!!fromIntegral ni==0 then 0 else ni+1
                      nbs = changeBs a ni bs
                   in ni':genX2 nbs as

genX :: Integer -> Integer -> [Integer]
genX 0 _ = []
genX i m = i `mod` m + 1 : genX (i-1) m  
  
genXIO :: Integer -> Integer -> IO [Integer]
genXIO 0 _ = return []
genXIO i m = do
  x <- randomRIO (0,m)  
  xs <- genXIO (i-1) m
  return (x:xs) 
