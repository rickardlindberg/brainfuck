import Brainfuck
import Prelude hiding (Left, Right)
import Test.Hspec.HUnit
import Test.Hspec.Monadic
import Test.HUnit

main = hspecX $ do

    describe "parse" $ do

        it "can parse simple" $ do
            parseTokens "+" @?= [TInc]
            parseTokens "-" @?= [TDec]

        it "can parse loops" $ do
            parseTokens ".[-]." @?= [TPrint, TLoop [TDec], TPrint]
