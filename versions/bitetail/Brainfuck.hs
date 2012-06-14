module Brainfuck where

import Prelude hiding (Left, Right)
import Data.Char (chr, ord)
import qualified Data.Map as M

data Data = Data
    { currentPos :: Int
    , values     :: M.Map Int Int
    }

emptyData :: Data
emptyData = Data 0 M.empty

dataGet :: Data -> Int
dataGet dat = M.findWithDefault 0 (currentPos dat) (values dat)

dataModifyValue :: Data -> (Int -> Int) -> Data
dataModifyValue dat fn = dat { values = newValues }
    where
        value     = M.findWithDefault 0 (currentPos dat) (values dat)
        newValues = M.insert (currentPos dat) (fn value) (values dat)

dataMoveRight :: Data -> Data
dataMoveRight = dataModifyPos (+1)

dataMoveLeft :: Data -> Data
dataMoveLeft = dataModifyPos (\x -> x - 1)

dataModifyPos :: (Int -> Int) -> Data -> Data
dataModifyPos fn dat = dat { currentPos = fn (currentPos dat) }



data Instruction
    = Inc
    | Dec
    | Left
    | Right
    | Print
    | Read
    | Loop [Instruction]
    deriving (Show, Eq)

parse :: String -> [Instruction]
parse input = let ("", instructios) = parseBlock input [] in instructios

parseBlock :: String -> [Instruction] -> (String, [Instruction])
parseBlock []                      opTail = ([], opTail)
parseBlock ('[':charsAfterOpening) opTail = (charTail, knot)
    where
        (charsAfterClosing, loopBody) = parseBlock charsAfterOpening knot
        (charTail, restOps)           = parseBlock charsAfterClosing opTail
        knot                          = (Loop loopBody):restOps
parseBlock (']':charsAfterClosing) opTail = (charsAfterClosing, opTail)
parseBlock (x:xs) opTail
    | x `elem` "+-<>.," = let (a, b) = parseBlock xs opTail in (a, parseSingle x:b)
    | otherwise         =                 parseBlock xs opTail

parseSingle :: Char -> Instruction
parseSingle '+' = Inc
parseSingle '-' = Dec
parseSingle '<' = Left
parseSingle '>' = Right
parseSingle '.' = Print
parseSingle ',' = Read

run :: [Instruction] -> String -> Data -> String
run []               input  dat = "done!\n"
run (Inc:next)       input  dat = run next input (dataModifyValue dat (+1))
run (Dec:next)       input  dat = run next input (dataModifyValue dat (\x -> x - 1))
run (Left:next)      input  dat = run next input (dataMoveLeft dat)
run (Right:next)     input  dat = run next input (dataMoveRight dat)
run (Print:next)     input  dat = chr (dataGet dat) : run next input dat
run (Read:next)      []     dat = error "no input"
run (Read:next)      (i:is) dat = run next is (dataModifyValue dat (const (ord i)))
run ((Loop xs):next) input  dat = if dataGet dat == 0
                                      then run next input dat
                                      else run xs input dat

execute :: String -> IO ()
execute program = interact (\input -> run (parse program) input emptyData)
