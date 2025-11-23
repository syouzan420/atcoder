import Data.List (sort)
main = getLine >>= putStrLn . unwords . sort . words
