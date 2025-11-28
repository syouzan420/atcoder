main = do
  [sa,op,sb] <- words <$> getLine
  let a = read sa :: Int; b = read sb :: Int 
  let res = case op of
              "+" -> a + b; "-" -> a - b
              "*" -> a * b; "/" -> a `div` b; _ -> 0
  if op=="?" then return () else print res >> main


