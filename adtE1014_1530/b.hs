import Control.Monad (replicateM)
import Data.List (isInfixOf)

rint :: IO Int
rint = readLn

main :: IO ()
main = do
  n <- readLn 
  ss <- replicateM n getLine 
  let b = ["sweet","sweet"] `isInfixOf` (init ss)
  let res = if b then "No" else "Yes" 
  putStrLn res
