main = do
    print $ take 10 makeBiteTail

makeBiteTail :: [Int]
makeBiteTail = 1:2:makeBiteTail
