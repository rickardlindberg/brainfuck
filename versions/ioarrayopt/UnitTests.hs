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
                [ MoveLeft
                , MoveRight
                , Increment
                , Decrement
                , Print
                , Read
                ]

        it "can parse a program with loops" $
            parse "+[-[.]]." @?=
                [ Increment
                , LoopStart 6
                , Decrement
                , LoopStart 5
                , Print
                , LoopEnd 3
                , LoopEnd 1
                , Print
                ]

        prop "creates one token per valid character" $
            forAll programWithOnlyValidChars $ \p ->
                length p == length (parse p)

    describe "io tape:" $ do

        it "initializes from list" $ do
            tape <- newIOTapeFromList [8]
            value <- tapeCurrentValue tape
            value @?= 8

        it "position can be moved left and right" $ do
            tape <- newIOTapeFromList [1, 2, 3]
            tapeMoveRight tape
            tapeMoveRight tape
            tapeMoveLeft tape
            value <- tapeCurrentValue tape
            value @?= 2

        it "position can be moved to exact position" $ do
            tape <- newIOTapeFromList [1, 2, 3]
            tapeMoveTo tape 2
            value <- tapeCurrentValue tape
            value @?= 3

        it "value can be modified" $ do
            tape <- newIOTapeFromList [9]
            tapeModify tape dec
            value <- tapeCurrentValue tape
            value @?= 8

programWithOnlyValidChars :: Gen String
programWithOnlyValidChars =
    oneof [ listOf (elements "<>+-.,")
          , do
              p <- programWithOnlyValidChars
              return $ "[" ++ p ++ "]"
          ]
