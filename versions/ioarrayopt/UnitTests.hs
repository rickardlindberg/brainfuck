import Brainfuck
import Test.Hspec.HUnit
import Test.Hspec.Monadic
import Test.Hspec.QuickCheck
import Test.HUnit
import Test.QuickCheck

main = hspecX $ do

    describe "parser:" $ do

        it "can parse a simple program" $
            parse "<>+-.," @?=
                [ MoveLeft 1
                , MoveRight 1
                , Increment 1
                , Decrement 1
                , Print
                , Read
                ]

        it "can parse a program with loops" $
            parse "+[-[.]]." @?=
                [ Increment 1
                , LoopStart 6
                , Decrement 1
                , LoopStart 5
                , Print
                , LoopEnd 3
                , LoopEnd 1
                , Print
                ]

    describe "optimizer:" $ do

        it "reduces multiple increments" $
            optimize [Increment 2, Increment 1] @?= [Increment 3]

    describe "io tape:" $ do

        it "initializes from list" $ do
            tape <- newIOTapeFromList [8]
            value <- tapeCurrentValue tape
            value @?= 8

        it "position can be moved left and right" $ do
            tape <- newIOTapeFromList [1, 2, 3]
            tapeMoveRightBy tape 1
            tapeMoveRightBy tape 1
            tapeMoveLeftBy tape 1
            value <- tapeCurrentValue tape
            value @?= 2

        it "position can be moved to exact position" $ do
            tape <- newIOTapeFromList [1, 2, 3]
            tapeMoveTo tape 2
            value <- tapeCurrentValue tape
            value @?= 3

        it "value can be modified" $ do
            tape <- newIOTapeFromList [9]
            tapeModify tape (+1)
            value <- tapeCurrentValue tape
            value @?= 10
