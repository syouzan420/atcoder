import Data.List (sort)
main = getLine >>= (\[x,y] -> if x==0 && y==0 then return () else putStrLn (show x++" "++show y) >> main) . sort . map (read :: String->Int) . words 
