module Brainfuck where

import Data.Char

data Tape = Tape {memBefore, memAfter :: [Int]}
    deriving (Show, Eq)

data Program = Program {before, after :: String}
    deriving (Show, Eq)

data Machine = Machine {
    tape :: Tape,
    output :: String,
    program :: Program }

increment (Tape before after) =
    Tape (before) ( head(after) + 1 : tail(after) )
decrement (Tape before after) =
    Tape (before) ( head(after) - 1 : tail(after) )
forward (Tape before [x]) =
    Tape (before ++ [x]) [0]
forward (Tape before after) =
    Tape (before ++ [head(after)]) (tail(after))
next (Program before after) =
    Program (before ++ [head(after)]) (tail(after))
back (Tape [] after) =
    Tape ([]) (after)
back (Tape before after) =
    Tape (init(before)) (last(before) : after)
printOut (Tape before after) =
    chr (head(after))

runProgram :: Machine -> Machine
runProgram (Machine tape output (Program before ('+' : rest))) =
    runProgram (Machine (increment tape) output (next(Program before ('+' : rest))))
runProgram (Machine tape output (Program before ('-' : rest))) =
    runProgram (Machine (decrement tape) output (next(Program before ('-' : rest))))
runProgram (Machine tape output (Program before ('.' : rest))) =
    runProgram (Machine tape (output ++ [printOut tape]) (next(Program before ('-' : rest))))
runProgram (Machine tape output (Program before "")) =
    (Machine tape output (Program before ""))
runProgram other =
    Machine (Tape [] []) "" (Program "" "")

execute :: String -> IO ()
execute program = do
    let machine = runProgram (Machine (Tape [] [0]) "" (Program "" program))
    if tape(machine) == (Tape [] [])
        then putStrLn "error!"
        else putStrLn $ output(machine) ++ "done!"
