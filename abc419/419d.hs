import Control.Monad (replicateM)
  
getInts :: Int -> IO [[Int]]
getInts n = replicateM n $ getLine >>= return . map read . words

main :: IO ()
main = do
  [[_,m]] <- getInts 1
  s <- getLine
  t <- getLine
  lrs <- getInts m 
  let lrts = map toLr lrs
  let tss = foldl change (s,t) lrts
  putStrLn (fst tss)     

toLr :: [Int] -> (Int,Int)
toLr [l,r] = (l,r)
toLr _ = (0,0)

change :: (String,String) -> (Int,Int) -> (String,String)
change (s,t) (l,r) = let sp = takeLR s (l,r)
                         tp = takeLR t (l,r)
                      in (repLR s (l,r) tp, repLR t (l,r) sp)
 
takeLR :: String -> (Int,Int) -> String
takeLR str (l,r) = take (r-l+1) $ drop (l-1) str

repLR :: String -> (Int,Int) -> String -> String
repLR str (l,r) rstr = take (l-1) str ++ rstr ++ drop r str

