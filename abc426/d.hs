import Control.Monad (replicateM_)
import qualified Data.ByteString.Char8 as B
import Data.List (unfoldr)

intsb :: IO [Int]
intsb = unfoldr (B.readInt . B.dropSpace) <$> B.getLine

intr :: IO Int
intr = readLn

main :: IO ()
main = do
  t <- intr 
  replicateM_ t cases

cases :: IO ()
cases = do
  n <- intr
  s <- getLine
  let res=0
  print res
  
