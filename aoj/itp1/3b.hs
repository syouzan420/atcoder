main = testcase 1

testcase :: Int -> IO ()
testcase i = getLine >>= \x -> if x=="0" then return () 
                else putStrLn ("Case "++show i++": "++x) >> testcase (i+1)
