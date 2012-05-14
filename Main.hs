module Main where

import Brainfuck
import Data.List
import System

main = do
    args <- getArgs
    case args of
        [program]
            | ".bf" `isSuffixOf` program -> readFile program >>= \program -> run (parse program) (emptyTape 0)
            | otherwise                  -> run (parse program) (emptyTape 0)
        _         -> putStrLn "usage: runhaskell Brainfuck.hs \"your program\""
