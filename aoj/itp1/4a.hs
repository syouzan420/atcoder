import Numeric (showFFloat)

main = do
  [a,b] <- map read . words <$> getLine :: IO [Int]
  let d = a `div` b
  let r = a `mod` b
  let f = if r==0 then show d 
                  else showFFloat (Just 5) (fromIntegral a / fromIntegral b :: Double) ""
  putStrLn $ show d ++" "++ show r ++" "++ f
