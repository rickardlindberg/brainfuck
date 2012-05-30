module Brainfuck where

import Data.Array.IO
import Data.Char
import Data.IORef
import System.IO

--

data Command =
      MoveRight Int
    | MoveLeft Int
    | Increment Int
    | Decrement Int
    | Print
    | Read
    | LoopStart Int
    | LoopEnd Int
    | Loop [Command]
    | NOP
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

tapeMoveRightBy :: IOTape a -> Int -> IO ()
tapeMoveRightBy tape n = modifyIORef (ioTapePos tape) (+n)

tapeMoveLeftBy :: IOTape a -> Int -> IO ()
tapeMoveLeftBy tape n = modifyIORef (ioTapePos tape) (\x -> x-n)

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

incBy :: Int -> Int -> Int
incBy n x = x + n

decBy :: Int -> Int -> Int
decBy n x = x - n

--

parse :: String -> [Command]
parse str = expandLoops $ optimize $ parseCommands (removeComments str)

removeComments :: String -> String
removeComments str = filter (`elem` "<>+-.,[]") str

parseCommands :: String -> [Command]
parseCommands []       = []
parseCommands ('<':xs) = MoveLeft  1 : parseCommands xs
parseCommands ('>':xs) = MoveRight 1 : parseCommands xs
parseCommands ('+':xs) = Increment 1 : parseCommands xs
parseCommands ('-':xs) = Decrement 1 : parseCommands xs
parseCommands ('.':xs) = Print       : parseCommands xs
parseCommands (',':xs) = Read        : parseCommands xs
parseCommands ('[':xs) = let (innerLoop, _:rest) = extractInnerLoop xs
                         in Loop (parseCommands innerLoop) : parseCommands rest

extractInnerLoop :: String -> (String, String)
extractInnerLoop tokens = extractInnerLoop' 0 [] tokens
    where
        extractInnerLoop' n acc [] = error "unexpected end of input"
        extractInnerLoop' n acc (x:xs)
            | x == '['             = extractInnerLoop' (n+1) (acc ++ [x]) xs
            | x == ']' && n == 0   = (acc, x:xs)
            | x == ']'             = extractInnerLoop' (n-1) (acc ++ [x]) xs
            | otherwise            = extractInnerLoop' n     (acc ++ [x]) xs

optimize :: [Command] -> [Command]
optimize []                                 = []
optimize ((MoveLeft  n):(MoveLeft  j):rest) = optimize (MoveLeft  (n+j) : rest)
optimize ((MoveRight n):(MoveRight j):rest) = optimize (MoveRight (n+j) : rest)
optimize ((Increment n):(Increment j):rest) = optimize (Increment (n+j) : rest)
optimize ((Decrement n):(Decrement j):rest) = optimize (Decrement (n+j) : rest)
optimize (Loop cmds:rest)                   = Loop (optimize cmds) : optimize rest
optimize (cmd:rest)                         = cmd : optimize rest

expandLoops :: [Command] -> [Command]
expandLoops commands = expandLoops' commands 0
    where
        expandLoops' []                 _ = []
        expandLoops' ((Loop cmds):rest) n = let loop  = expandLoops' cmds (n+1)
                                                loopStartN = n
                                                afterLoopN = n + length loop + 2
                                                loopEndN = afterLoopN - 1
                                            in LoopStart loopEndN : loop ++ [LoopEnd loopStartN] ++ expandLoops' rest afterLoopN
        expandLoops' (cmd:rest)         n = cmd : expandLoops' rest (n + 1)

--

execute :: String -> IO ()
execute str = do
    prog <- newIOTapeFromList $ parse str ++ [NOP]
    dat  <- newIOTapeFromList [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    let loop = do
        let continue     = tapeMoveRightBy prog  1  >> loop
        let continueAt n = tapeMoveTo    prog n >> loop
        command <- tapeCurrentValue prog
        case command of
            MoveRight n -> tapeMoveRightBy dat n         >> continue
            MoveLeft  n -> tapeMoveLeftBy  dat n         >> continue
            Increment n -> tapeModify      dat (incBy n) >> continue
            Decrement n -> tapeModify      dat (decBy n) >> continue
            Print       -> tapeCurrentValue dat >>= (putChar . chr) >> hFlush stdout
                                                         >> continue
            Read        -> getChar >>= (\v -> tapeModify dat (const $ ord $ v))
                                                         >> continue
            LoopStart n -> tapeCurrentValue dat >>= \v -> if v == 0 then continueAt n else continue
            LoopEnd n   -> tapeCurrentValue dat >>= \v -> if v == 0 then continue     else continueAt n
            NOP         -> putStrLn "done!"
    loop
