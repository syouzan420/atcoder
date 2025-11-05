{-# LANGUAGE TypeApplications #-}
import Control.Monad (replicateM)
  
getInts :: Int -> IO [[Int]]
getInts n = replicateM n $ getLine >>= return . map read . words

main :: IO ()
main = do
  n <- readLn @Int
  rcs <- getInts n >>= return . map toRC
  let (rs,cs) = unzip rcs
  let mars = maximum rs 
  let mirs = minimum rs
  let midrs = ceiling $ fromIntegral (mars-mirs) / 2
  let macs = maximum cs 
  let mics = minimum cs
  let midcs = ceiling $ fromIntegral (macs-mics) / 2
  let res = max midrs midcs 
  print res    
  
toRC :: [Int] -> (Int,Int)
toRC [a,b] = (a,b)
toRC _ = (0,0)
