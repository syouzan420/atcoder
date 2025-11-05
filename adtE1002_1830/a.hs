import Data.List (nub)

ints :: IO [Int]
ints = map read . words <$> getLine

main :: IO ()
main = do
 [a,b] <- ints 
 let as = ptol a
 let bs = ptol b
 let res = sum $ nub $ as++bs 
 print res

ptol :: Int -> [Int]
ptol 1 = [1]
ptol 2 = [2]
ptol 3 = [1,2]
ptol 4 = [4]
ptol 5 = [2,3]
ptol 6 = [2,4]
ptol 7 = [1,2,4]
ptol _ = []
