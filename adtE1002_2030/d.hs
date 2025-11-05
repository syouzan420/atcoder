import Control.Monad (replicateM)

getint :: IO Int
getint = readLn

main :: IO ()
main = do
  n <- getint
  ss <- replicateM n getLine
  let ib = check ss 
  let ib2 = if ib then
                if length ss == 1 then True else
                      if (head ss `elem` tail ss) then False else True
                  else False
  let res = if ib2 then "Yes" else "No" 
  putStrLn res

check :: [String] -> Bool
check [] = True 
check (s:xs) = let (h,l) = case s of [h',l'] -> (h',l'); _ -> (' ',' ')
                in if not (h `elem` "HDCS") then False
                    else if not (l `elem` "A23456789TJQK") then False 
                      else check xs
