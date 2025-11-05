geti :: IO Int
geti = readLn

main :: IO ()
main = do
  _ <- geti
  s <- getLine
  let res = count s 
  print res 

count :: String -> Int
count [] = 0
count [_] = 0
count [_,_] = 0
count (ch0:ch1:ch2:xs) =
   let ib = ch0=='#' && ch1=='.' && ch2=='#'
       next = count (ch1:ch2:xs)
    in if ib then 1+next else next 
