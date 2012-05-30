module Brainfuck where

import Data.Array.IO
import Data.Char
import Data.IORef
import System.IO

--

data Command =
      MoveRight
    | MoveLeft
    | Increment
    | Decrement
    | Print
    | Read
    | LoopStart Int
    | LoopEnd Int
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

tapeMoveRight :: IOTape a -> IO ()
tapeMoveRight tape = modifyIORef (ioTapePos tape) inc

tapeMoveLeft :: IOTape a -> IO ()
tapeMoveLeft tape = modifyIORef (ioTapePos tape) dec

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

inc :: Int -> Int
inc x = x + 1

dec :: Int -> Int
dec x = x - 1

--

type Token = (Int, Char)

parse :: String -> [Command]
parse str = parseTokens (tokenize str)

tokenize :: String -> [Token]
tokenize str = zip [0..length validCharacters - 1] validCharacters
    where
        validCharacters = filter (`elem` "<>+-.,[]") str

parseTokens :: [Token] -> [Command]
parseTokens []            = []
parseTokens ((_, '<'):xs) = MoveLeft  : parseTokens xs
parseTokens ((_, '>'):xs) = MoveRight : parseTokens xs
parseTokens ((_, '+'):xs) = Increment : parseTokens xs
parseTokens ((_, '-'):xs) = Decrement : parseTokens xs
parseTokens ((_, '.'):xs) = Print     : parseTokens xs
parseTokens ((_, ','):xs) = Read      : parseTokens xs
parseTokens ((n, '['):xs) = let (innerLoop, (n',_):rest) = extractInnerLoop xs
                            in ((LoopStart n') : parseTokens innerLoop) ++ [LoopEnd n] ++ parseTokens rest

extractInnerLoop :: [Token] -> ([Token], [Token])
extractInnerLoop tokens = extractInnerLoop' 0 [] tokens
    where
        extractInnerLoop' n acc [] = error "unexpected end of input"
        extractInnerLoop' n acc ((j, x):xs)
            | x == '['             = extractInnerLoop' (n+1) (acc ++ [(j, x)]) xs
            | x == ']' && n == 0   = (acc, (j, x):xs)
            | x == ']'             = extractInnerLoop' (n-1) (acc ++ [(j, x)]) xs
            | otherwise            = extractInnerLoop' n     (acc ++ [(j, x)]) xs

--

execute :: String -> IO ()
execute str = do
    prog <- newIOTapeFromList $ parse str ++ [NOP]
    dat  <- newIOTapeFromList [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    let loop = do
        let continue     = tapeMoveRight prog   >> loop
        let continueAt n = tapeMoveTo    prog n >> loop
        command <- tapeCurrentValue prog
        case command of
            MoveRight   -> tapeMoveRight dat     >> continue
            MoveLeft    -> tapeMoveLeft  dat     >> continue
            Increment   -> tapeModify    dat inc >> continue
            Decrement   -> tapeModify    dat dec >> continue
            Print       -> tapeCurrentValue dat >>= (putChar . chr) >> hFlush stdout >> continue
            Read        -> getChar >>= (\v -> tapeModify dat (const $ ord $ v)) >> continue
            LoopStart n -> tapeCurrentValue dat >>= \v -> if v == 0 then continueAt n else continue
            LoopEnd n   -> tapeCurrentValue dat >>= \v -> if v == 0 then continue     else continueAt n
            NOP         -> putStrLn "done!"
    loop
