import Control.Monad (replicateM)

main :: IO ()
main = do
  n <- readLn :: IO Int 
  ss <- replicateM n $ getLine
  putStr $ unlines $ reverse ss
