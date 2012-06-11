module Brainfuck where

import qualified Data.Map as M
import Data.Maybe
import Data.Char

data Op = MLeft
        | MRight
        | Inc
        | Dec
        | In
        | Out
    deriving (Show, Eq)

type Program = [Op]
type Input = [Int]
type Output = [Int]
type Position = Int
type Tape = M.Map Int Int
data Machine = Machine
    { input    :: Input
    , position :: Position
    , tape     :: Tape
    } deriving (Show, Eq)

parseOp :: Char -> Maybe Op
parseOp '<' = Just MLeft
parseOp '>' = Just MRight
parseOp '+' = Just Inc
parseOp '-' = Just Dec
parseOp '.' = Just Out
parseOp ',' = Just In
parseOp _   = Nothing

parseProgram :: [Char] -> [Op]
parseProgram program = mapMaybe parseOp program

withDefault :: a -> (a -> a) -> Maybe a -> Maybe a
withDefault def f (Just old) = Just (f old) 
withDefault def f Nothing    = Just (f def) 

executeOp :: Op -> Machine -> (Machine, Maybe Int)
executeOp MLeft  machine = (machine { position = position machine - 1 }, Nothing)
executeOp MRight machine = (machine { position = position machine + 1 }, Nothing)
executeOp Inc    machine =
    (machine { tape = M.alter (withDefault 0 (+1)) (position machine) (tape machine) },
     Nothing)
executeOp Dec    machine =
    (machine { tape = M.alter (withDefault 0 (subtract 1)) (position machine) (tape machine) },
     Nothing)
executeOp In     machine@Machine { input=(x:xs), tape=tape, position=position } =
    (machine { tape  = M.insert position x tape,
               input = xs },
     Nothing)
executeOp Out    machine@Machine { tape=tape, position=position } = 
    (machine, Just (M.findWithDefault 0 position tape))

initialMachine :: Input -> Machine
initialMachine input = Machine input 0 M.empty

executeProgram :: Machine -> Program -> Output
executeProgram _       []       = []
executeProgram machine (op:ops) =
    let (newMachine,output) = executeOp op machine
    in case output of
           Just x  -> x:(executeProgram newMachine ops)
           Nothing -> executeProgram newMachine ops

execute' :: String -> String -> String
execute' program input =
    map chr $ executeProgram (initialMachine (map ord input)) (parseProgram program)

execute :: String -> IO ()
execute program =
    interact (execute' program) >> putStrLn "done!"
