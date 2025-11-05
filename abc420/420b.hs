import Control.Monad (replicateM)
import Data.List (transpose,elemIndices,intersperse)
  
getStrs :: Int -> IO [String]
getStrs n = replicateM n $ getLine

main :: IO ()
main = do
  [n,_] <- getLine >>= return . map read . words
  ss <- getStrs n
  let bls = map toBList ss
  let trbls = transpose bls
  let inp = replicate n (0::Int)
  let ps = foldl point inp trbls
  let mxp = maximum ps
  let inds = elemIndices mxp ps
  let rsInds = map (+1) inds
  let res = intersperse ' ' $ concatMap show rsInds
  print res     

toBList :: String -> [Bool]
toBList [] = []
toBList (x:xs) = if x=='1' then True:toBList xs else False:toBList xs

point :: [Int] -> [Bool] -> [Int]
point pp bls = let ft = filter id bls   
                   lng = length bls
                   flng = length ft
                   gbool = flng<(lng-flng) 
                   isZero = lng==flng || flng==0
                   zipPB = zip pp bls
                in if isZero then map (+1) pp 
                    else map (\(p,b) -> if b==gbool then p+1 else p) zipPB
