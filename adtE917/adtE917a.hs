import Data.Functor ((<&>))

main :: IO ()
main = do
  [s,_] <- getLine <&> words
  let res = s ++ " san"
  putStrLn res
