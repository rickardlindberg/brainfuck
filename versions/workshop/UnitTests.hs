import Brainfuck
import qualified Data.Map as M
import Test.Hspec.HUnit
import Test.Hspec.Monadic
import Test.HUnit

main = hspecX $ do

    describe "tape" $ do

        it "creation of empty tape" $ do
            emptyTape 0 @?= Tape { currentPos   = 0
                                 , values       = M.empty
                                 , defaultValue = 0
                                 }
