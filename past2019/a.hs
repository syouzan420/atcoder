import Data.Char (isDigit)

main :: IO ()
main = do
  s <- getLine
  let b = and $ map isDigit s
  let res = if b then show ((read s :: Int)*2)
                 else "error"
  putStrLn res

