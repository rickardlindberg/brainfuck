module Brainfuck where

import Prelude hiding (Left, Right)
import Data.Char (chr, ord)
import qualified Data.Map as M

data DataMap = DataMap
    { currentPos :: Int
    , values     :: M.Map Int Int
    }

dataMoveRight :: Data d => d -> d
dataMoveRight = dataModifyPos (+1)

dataMoveLeft :: Data d => d -> d
dataMoveLeft = dataModifyPos (\x -> x - 1)

class Data d where
    emptyData       :: d
    dataGet         :: d -> Int
    dataModifyValue :: d -> (Int -> Int) -> d
    dataModifyPos   :: (Int -> Int) -> d -> d

instance Data DataMap where
--    emptyData :: DataMap
    emptyData = DataMap 0 M.empty

--    dataGet :: DataMap -> Int
    dataGet dat = M.findWithDefault 0 (currentPos dat) (values dat)

--    dataModifyValue :: DataMap -> (Int -> Int) -> DataMap
    dataModifyValue dat fn = dat { values = newValues }
        where
            value     = M.findWithDefault 0 (currentPos dat) (values dat)
            newValues = M.insert (currentPos dat) (fn value) (values dat)

--    dataModifyPos :: (Int -> Int) -> DataMap -> DataMap
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

run :: Data d => [Instruction] -> String -> d -> String
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
execute program = interact (\input -> run (parse program) input emptyDataMap)

emptyDataMap :: DataMap
emptyDataMap = emptyData
