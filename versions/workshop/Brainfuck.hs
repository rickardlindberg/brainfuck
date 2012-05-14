module Brainfuck where

import Data.Char
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

data Tape a = Tape
    { currentPos   :: Int
    , values       :: M.Map Int a
    , defaultValue :: a
    } deriving (Show, Eq)

emptyTape :: a -> Tape a
emptyTape def = Tape 0 M.empty def

--

type Program = Tape Command

type Data = Tape Int

--

parse :: String -> Program
parse str = undefined

--

run :: Program -> Data -> IO ()
run p d = putStrLn "done!"

--

execute :: String -> IO ()
execute program = run (parse program) (emptyTape 0)
