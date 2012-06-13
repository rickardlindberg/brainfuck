import Brainfuck
import Test.Hspec.HUnit
import Test.Hspec.Monadic
import Test.HUnit

anEmptyTape = (Tape [] [0])
aMachineWithProgram p = (Machine anEmptyTape "" (Program "" p))

main = hspecX $ do


    describe "test increment" $ do
        it "can increment once" $ do
            increment anEmptyTape @?= (Tape [] [1])
        it "can increment twice" $ do
            increment ( increment anEmptyTape) @?= (Tape [] [2])
        it "can increment twice" $ do
            increment ( increment anEmptyTape) @?= (Tape [] [2])

    describe "decrement" $ do
        it "can decrement once" $ do
            decrement (Tape [] [1]) @?= (Tape [] [0])

    describe "print" $ do
        it "prints the first item" $ do
            printOut (Tape [] [65]) @?= 'A'

    describe "move forward" $ do
        it "takes one step forward" $ do
            forward (Tape [] [0, 1]) @?= (Tape [0] [1])
        it "takes one step forward and appends zero at end" $ do
            forward (Tape [] [0]) @?= (Tape [0] [0])

    describe "move back" $ do
        it "takes one step back" $ do
            back (Tape [0] [1]) @?= (Tape [] [0, 1])
        it "stops at beginning" $ do
            back (Tape [] [0]) @?= (Tape [] [0])

    describe "runProgram" $ do
        it "runs a single increment" $ do
            tape(runProgram (aMachineWithProgram "+")) @?= (Tape [] [1])
        it "runs two increments" $ do
            tape(runProgram (aMachineWithProgram "++")) @?= (Tape [] [2])
        it "increments and decrements back to zero" $ do
            tape(runProgram (aMachineWithProgram "++--")) @?= anEmptyTape
        it "prints the first item" $ do
            output(runProgram (Machine (Tape [] [65]) "" (Program "" "."))) @?= "A"
