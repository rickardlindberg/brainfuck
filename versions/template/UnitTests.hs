import Brainfuck
import Test.Hspec.HUnit
import Test.Hspec.Monadic
import Test.HUnit

main = hspecX $ do

    describe "a unit test" $ do

        it "can be written here" $ do
            aFunction "replace me" @?= 10
