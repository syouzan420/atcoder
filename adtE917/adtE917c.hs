import Data.Functor ((<&>))
import Data.List (isPrefixOf,isSuffixOf)

main :: IO ()
main = do
 [_,_] <- getLine <&> words 
 s <- getLine
 t <- getLine
 let ip = isPrefixOf s t
 let is = isSuffixOf s t
 let res
      | ip && is = 0 :: Int
      | ip = 1
      | is = 2
      | otherwise = 3
 print res
