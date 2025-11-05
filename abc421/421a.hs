{-# LANGUAGE ScopedTypeVariables #-}
import Control.Monad (replicateM)

main :: IO ()
main = do
  n :: Int <- getLine >>= return . read 
  ss <- replicateM n $ getLine
  [x',y] <- getLine >>= return . words
  let x :: Int = read x'
  let res = isLiving x y ss
  putStrLn $ if res then "Yes" else "No"

isLiving :: Int -> String -> [String] -> Bool
isLiving x y ss = ss!!(x-1) == y
