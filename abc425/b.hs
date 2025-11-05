import Data.List (nub)

ints :: IO [Int]
ints = map read . words <$> getLine

main :: IO ()
main = do
  n <- readLn
  as <- ints
  let (ib,p) = isP n as
  let res1 = if ib then "Yes" else "No" 
  let res2 = unwords $ map show p 
  if ib then mapM_ putStrLn [res1,res2] else putStrLn res1

isP :: Int -> [Int] -> (Bool,[Int])
isP n as = let nms = filter (/=(-1)) as
               nbnms = nub nms
               ots = other nms [1..n]
               p = getin as ots 
            in (nms == nbnms, p)

other :: [Int] -> [Int] -> [Int] 
other _ [] = []
other nms (x:xs) = if x `elem` nms then other nms xs else x:other nms xs 

getin :: [Int] -> [Int] -> [Int]
getin as [] = as 
getin [] _ = []
getin (a:as) (x:xs) = if a==(-1) then x:getin as xs else a:getin as (x:xs)

