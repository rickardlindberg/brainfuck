module Main where

import Brainfuck
import Data.List
import System.Environment

main = do
    args <- getArgs
    case args of
        [program]
            | ".bf" `isSuffixOf` program -> readFile program >>= execute
            | otherwise                  -> execute program
        _         -> putStrLn "usage: runhaskell Brainfuck.hs \"your program\""
