import Brainfuck
import Test.Hspec.HUnit
import Test.Hspec.Monadic
import Test.HUnit

main = hspecX $ do

    describe "parsing" $ do

        it "can parse a simple program" $ do
            parse "+." @?= ( tapePut 1 Print
                           $ tapePut 0 Increment
                           $ (emptyTape NOP)
                           )
