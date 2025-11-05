{-# LANGUAGE OverloadedStrings #-}

import Data.List (isInfixOf)
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T 
import qualified Data.Text.Encoding as E 

main :: IO ()
main = do
  _ <- getLine 
  s <- B.getLine >>= return . E.decodeLatin1 
  let (res,_) = change (0,s)
  print res 

change :: (Int,T.Text) -> (Int,T.Text)
change (i,s)
        | isAAB s = change (i+1,T.replace "AAB" "ABA" s)
        | isBBA s = change (i+1,T.replace "BBA" "BAB" s)
        | otherwise = (i,s)

isAAB :: T.Text -> Bool
isAAB = T.isInfixOf "AAB" 

isBBA :: T.Text -> Bool
isBBA = T.isInfixOf "BBA"
