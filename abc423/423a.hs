import System.IO (stdout,hFlush)

main :: IO ()
main = do
  [x,c] <- getLine >>= return . map (\i -> read i::Int) . words
  let res = show $ canget (fromIntegral x) (fromIntegral c) 
  putStrLn res
  hFlush stdout
  
canget :: Float -> Float -> Int
canget x c = let cg = floor $ x / (1000+c)  
              in cg*1000  
                 
