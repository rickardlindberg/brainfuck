module Brainfuck where

aFunction :: [a] -> Int
aFunction x = length x

execute :: String -> IO ()
execute program = putStrLn "done!"
