data Dr = E | S | W | N

main :: IO ()
main = do
  _ <- readLn :: IO Int 
  t <- getLine
  let (_,p) = getPos (E,(0,0)) t 
  let res = show (fst p) ++ " " ++ show (snd p) 
  putStrLn res

getPos :: (Dr,(Int,Int)) -> String -> (Dr,(Int,Int))
getPos dps [] = dps
getPos (d,ps) (x:xs) =
  if x=='S' then getPos (d,move d ps) xs else getPos (rota d,ps) xs

move :: Dr -> (Int,Int) -> (Int,Int)                              
move E (x,y) = (x+1,y)
move S (x,y) = (x,y-1)
move W (x,y) = (x-1,y)
move N (x,y) = (x,y+1)

rota :: Dr -> Dr
rota E = S
rota S = W
rota W = N
rota N = E

