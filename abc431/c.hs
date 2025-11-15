import Data.List (sort)
import qualified Data.ByteString.Char8 as B
import Data.List (unfoldr)


ints :: IO [Int]
ints = unfoldr (B.readInt . B.dropSpace) <$> B.getLine

main :: IO ()
main = do
  [n,m,k] <- ints
  hs <- ints
  bs <- ints
  let shs = take k $ sort hs 
  let sbs = sort bs
  let bl = check shs sbs 
  putStrLn $ if bl then "Yes" else "No"

check :: [Int] -> [Int] -> Bool
check [] _ = True
check _ [] = False
check shs@(h:hs) (b:bs) = if b>=h then check hs bs 
                                  else check shs bs 
