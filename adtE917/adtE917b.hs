import Control.Monad (replicateM)

main :: IO ()
main = do
 ss <- replicateM 12 getLine
 let res = numberS 12 ss
 putStrLn (show res)


numberS :: Int -> [String] -> Int
numberS _ [] = 0
numberS i (x:xs) = let ad = if length x == i then 1 else 0
                    in ad+numberS (i-1) xs
