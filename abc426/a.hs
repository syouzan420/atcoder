import Data.List (elemIndex)

main :: IO ()
main = do
  [x,y] <- words <$> getLine 
  let vs = ["Ocelot","Serval","Lynx"]
  let ib = check vs x y 
  let res = if ib then "Yes" else "No"
  putStrLn res

check :: [String] -> String -> String -> Bool
check vs x y = let xi = elemIndex x vs 
                   yi = elemIndex y vs
                in xi>=yi
