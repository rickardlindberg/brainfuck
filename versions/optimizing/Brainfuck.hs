module Brainfuck where

import Data.Array.IO
import Data.Char
import Data.IORef
import System.IO

--

data Program =
      MoveBy Int
    | ModifyBy Int
    | Print
    | Read
    | Loop [Program]
    deriving (Show, Eq)

data ByteCode =
      BMoveBy Int
    | BModifyBy Int
    | BPrint
    | BRead
    | BLoopStart Int
    | BLoopEnd Int
    | BNOP
    deriving (Show, Eq)

--

data IOTape a = IOTape
    { ioTapePos   :: IORef Int
    , ioTapeArray :: IOArray Int a
    }

newIOTapeFromList :: [a] -> IO (IOTape a)
newIOTapeFromList list = do
    pos <- newIORef 0
    arr <- newListArray (0, length list - 1) list
    return $ IOTape pos arr

tapeMoveBy :: IOTape a -> Int -> IO ()
tapeMoveBy tape n = modifyIORef (ioTapePos tape) (+n)

tapeMoveTo :: IOTape a -> Int -> IO ()
tapeMoveTo tape n = writeIORef (ioTapePos tape) n

tapeModify :: IOTape a -> (a -> a) -> IO ()
tapeModify tape fn = do
    index <- readIORef (ioTapePos tape)
    value <- readArray (ioTapeArray tape) index
    writeArray (ioTapeArray tape) index (fn value)

tapeCurrentValue :: IOTape a -> IO a
tapeCurrentValue tape = do
    index <- readIORef (ioTapePos tape)
    value <- readArray (ioTapeArray tape) index
    return value

--

compile :: String -> [ByteCode]
compile = append BNOP . toByteCode . optimize . parse

parse :: String -> [Program]
parse []       = []
parse (x:xs) | x `notElem` "<>+-.,[]" = parse xs
parse ('<':xs) = MoveBy   (-1) : parse xs
parse ('>':xs) = MoveBy     1  : parse xs
parse ('+':xs) = ModifyBy   1  : parse xs
parse ('-':xs) = ModifyBy (-1) : parse xs
parse ('.':xs) = Print         : parse xs
parse (',':xs) = Read          : parse xs
parse ('[':xs) = let (innerLoop, ']':rest) = extractInnerLoop xs
                 in Loop (parse innerLoop) : parse rest
--parse (_  :xs) = parse xs

extractInnerLoop :: String -> (String, String)
extractInnerLoop tokens = extractInnerLoop' 0 [] tokens
    where
        extractInnerLoop' n acc [] = error "unexpected end of input"
        extractInnerLoop' n acc (x:xs)
            | x == '['             = extractInnerLoop' (n+1) (acc ++ [x]) xs
            | x == ']' && n == 0   = (acc, x:xs)
            | x == ']'             = extractInnerLoop' (n-1) (acc ++ [x]) xs
            | otherwise            = extractInnerLoop' n     (acc ++ [x]) xs

optimize :: [Program] -> [Program]
optimize []                               = []
optimize ((MoveBy   a):(MoveBy   b):rest) = optimize (MoveBy   (a+b) : rest)
optimize ((ModifyBy a):(ModifyBy b):rest) = optimize (ModifyBy (a+b) : rest)
optimize (Loop cmds                :rest) = Loop (optimize cmds) : optimize rest
optimize (cmd                      :rest) = cmd                  : optimize rest

toByteCode :: [Program] -> [ByteCode]
toByteCode commands = toByteCode' commands 0
    where
        toByteCode' []                 _ = []
        toByteCode' (MoveBy x   :rest) n = BMoveBy x   : toByteCode' rest (n + 1)
        toByteCode' (ModifyBy x :rest) n = BModifyBy x : toByteCode' rest (n + 1)
        toByteCode' (Print      :rest) n = BPrint      : toByteCode' rest (n + 1)
        toByteCode' (Read       :rest) n = BRead       : toByteCode' rest (n + 1)
        toByteCode' ((Loop cmds):rest) n = let loop  = toByteCode' cmds (n+1)
                                               loopStartN = n
                                               afterLoopN = n + length loop + 2
                                               loopEndN = afterLoopN - 1
                                           in BLoopStart loopEndN : loop ++ [BLoopEnd loopStartN] ++ toByteCode' rest afterLoopN

append :: a -> [a] -> [a]
append item list = list ++ [item]

--

execute :: String -> IO ()
execute str = do
    prog <- newIOTapeFromList $ compile str
    dat  <- newIOTapeFromList [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    let loop = do
        let continue     = tapeMoveBy prog 1 >> loop
        let continueAt n = tapeMoveTo prog n >> loop
        command <- tapeCurrentValue prog
        case command of
            BMoveBy n    -> tapeMoveBy dat n         >> continue
            BModifyBy n  -> tapeModify dat (+n)      >> continue
            BPrint       -> tapeCurrentValue dat >>= (putChar . chr) >> hFlush stdout
                                                     >> continue
            BRead        -> getChar >>= (\v -> tapeModify dat (const $ ord $ v))
                                                     >> continue
            BLoopStart n -> tapeCurrentValue dat >>= \v -> if v == 0 then continueAt n else continue
            BLoopEnd n   -> tapeCurrentValue dat >>= \v -> if v == 0 then continue     else continueAt n
            BNOP         -> putStrLn "done!"
    loop
