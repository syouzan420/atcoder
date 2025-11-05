import Data.Functor ((<&>))

main :: IO ()
main = do
  [x,y] <- getLine <&> map read . words
  let a = [1,2,3,4,5,6]
  let xs = [(p,q)|p<-a, q<-a, p+q>=x || abs (p-q)>=y]
  let len = length xs
  let res = (fromIntegral len / 36)::Double 
  print res

  
