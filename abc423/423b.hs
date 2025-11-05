import System.IO (stdout,hFlush)

main :: IO ()
main = do
  _ <- getLine
  ls <- getLine >>= return . concat . words
  let res= show $ findRooms ls
  putStrLn res
  hFlush stdout

fClose :: String -> Int
fClose [] = -1
fClose (x:xs) = if x=='1' then 0 else 1 + fClose xs

findRooms :: String -> Int
findRooms str = let isOne = '1' `elem` str
                    lng = length str
                    fromLeft = fClose str
                    fromRight = fClose (reverse str)
                 in if not isOne then 0   
                                 else lng - fromLeft - fromRight - 1 
