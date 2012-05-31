import Brainfuck
import Test.Hspec.HUnit
import Test.Hspec.Monadic
import Test.Hspec.QuickCheck
import Test.HUnit
import Test.QuickCheck

main = hspecX $ do

    describe "parser:" $ do

        it "can parse a simple program" $
            parse "<+>-.," @?=
                [ MoveBy (-1)
                , ModifyBy 1
                , MoveBy 1
                , ModifyBy (-1)
                , Print
                , Read
                ]

        it "can parse a program with loops" $
            parse "+[-[.]]." @?=
                [ ModifyBy 1
                , Loop [ ModifyBy (-1)
                       , Loop [ Print ]
                       ]
                , Print
                ]

        prop "parses to the same thing if comments included" $
            forAll aProgram $ \(p, pWithComments) ->
                parse p == parse pWithComments

    describe "compiler:" $ do

        it "can compile a simple program" $
            compile "<+>-.," @?=
                [ BMoveBy (-1)
                , BModifyBy 1
                , BMoveBy 1
                , BModifyBy (-1)
                , BPrint
                , BRead
                , BNOP
                ]

        it "can compile a program with loops" $
            compile "+[-[.]]." @?=
                [ BModifyBy 1
                , BLoopStart 6
                , BModifyBy (-1)
                , BLoopStart 5
                , BPrint
                , BLoopEnd 3
                , BLoopEnd 1
                , BPrint
                , BNOP
                ]

    describe "optimizer:" $ do

        it "reduces multiple increments" $
            optimize [ModifyBy 2, ModifyBy 1] @?= [ModifyBy 3]

    describe "io tape:" $ do

        it "initializes from list" $ do
            tape <- newIOTapeFromList [8]
            value <- tapeCurrentValue tape
            value @?= 8

        it "position can be moved left and right" $ do
            tape <- newIOTapeFromList [1, 2, 3]
            tapeMoveBy tape 1
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

aProgram :: Gen (String, String)
aProgram = do
    p <- oneof [ listOf (elements "<>+-.,")
               , do
                   (innerP, _) <- aProgram
                   return $ "[" ++ innerP ++ "]"
               ]
    comments <- vectorOf (length p) (listOf (elements " #\n=/"))
    let programWithComments = concatMap (\(p, c) -> p:c) (zip p comments)
    return (p, programWithComments)
