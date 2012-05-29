module Brainfuck where

import Data.Array.IO
import Data.Char
import Data.IORef
import Data.Maybe
import qualified Data.Map as M
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
    { ioTapeValues :: IORef (M.Map Int a)
    , ioTapePos    :: IORef Int
    }

newIOTapeFromList :: [a] -> IO (IOTape a)
newIOTapeFromList list = do
    values <- newIORef (M.fromList (zip [0..length list] list))
    pos <- newIORef 0
    return $ IOTape values pos

tapeMoveRight :: IOTape a -> IO ()
tapeMoveRight tape = modifyIORef (ioTapePos tape) inc

tapeMoveLeft :: IOTape a -> IO ()
tapeMoveLeft tape = modifyIORef (ioTapePos tape) dec

tapeMoveTo :: IOTape a -> Int -> IO ()
tapeMoveTo tape n = writeIORef (ioTapePos tape) n

ioTapeModify :: IOTape a -> (a -> a) -> IO ()
ioTapeModify tape fn = do
    index <- readIORef (ioTapePos tape)
    modifyIORef (ioTapeValues tape) (\map ->
        let value = fromJust $ M.lookup index map
        in M.insert index (fn value) map)

ioTapeValue :: IOTape a -> IO a
ioTapeValue tape = do
    index <- readIORef (ioTapePos tape)
    map <- readIORef (ioTapeValues tape)
    return $ fromJust $ M.lookup index map

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
tokenize str = zip [0..length f - 1] f
    where
        f = filter (`elem`"<>+-.,[]") str

parseTokens :: [Token] -> [Command]
parseTokens ((n, '<'):xs) = MoveLeft  : parseTokens xs
parseTokens ((n, '>'):xs) = MoveRight : parseTokens xs
parseTokens ((n, '+'):xs) = Increment : parseTokens xs
parseTokens ((n, '-'):xs) = Decrement : parseTokens xs
parseTokens ((n, '.'):xs) = Print     : parseTokens xs
parseTokens ((n, ','):xs) = Read      : parseTokens xs
parseTokens ((n, '['):xs) = let (innerLoop, (n',_):rest) = extractInnerLoop xs
                               in ((LoopStart n') : parseTokens innerLoop) ++ [LoopEnd n] ++ parseTokens rest
parseTokens x = []

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
    dat <- newIOTapeFromList [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    let loop = do
        let continue     = tapeMoveRight prog   >> loop
        let continueAt n = tapeMoveTo    prog n >> loop
        command <- ioTapeValue prog
        case command of
            MoveRight   -> tapeMoveRight dat     >> continue
            MoveLeft    -> tapeMoveLeft  dat     >> continue
            Increment   -> ioTapeModify  dat inc >> continue
            Decrement   -> ioTapeModify  dat dec >> continue
            Print       -> ioTapeValue dat >>= (putChar . chr) >> hFlush stdout >> continue
            Read        -> getChar >>= (\v -> ioTapeModify dat (const $ ord $ v)) >> continue
            LoopStart n -> ioTapeValue dat >>= \v -> if v == 0 then continueAt n else continue
            LoopEnd n   -> ioTapeValue dat >>= \v -> if v == 0 then continue     else continueAt n
            NOP         -> putStrLn "done!"
    loop
