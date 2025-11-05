import Data.Functor ((<&>))

ints :: IO [Int]
ints = getLine <&> map read . words

main :: IO ()
main = do
  [a,b] <- ints 
  let (c,d) = dis1 (fromIntegral a) (fromIntegral b) 
  let res = show c ++ " " ++ show d
  putStrLn res

dis1 :: Double -> Double -> (Double,Double)
dis1 a b = let d = sqrt $ a^2+b^2
            in (a/d,b/d)
