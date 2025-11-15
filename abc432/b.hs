import Data.List (sort,uncons)

main :: IO ()
main = do
  x <- readLn :: IO Int
  let sx = sort $ show x
  let (zeros,nonzeros) = span (=='0') sx 
  let (h,tl) = case uncons nonzeros of
                  Nothing -> (' ',"")
                  Just (h',tl') -> (h',tl')
  let res = if h==' ' then "0" else (h:zeros)++tl
  putStrLn res

