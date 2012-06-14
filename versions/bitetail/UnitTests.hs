import Brainfuck
import Prelude hiding (Left, Right)
import Test.Hspec.HUnit
import Test.Hspec.Monadic
import Test.HUnit

main = hspecX $ do

    describe "parsing" $ do

        it "simple instructions" $ do
            parse "+" @?= [Inc]
            parse "-" @?= [Dec]
            parse "<" @?= [Left]
            parse ">" @?= [Right]
            parse "," @?= [Read]
            parse "." @?= [Print]

        it "loops" $ do
            let [Inc, loop, Print] = parse "+[->+<]."
            let (Loop loopList) = loop

            loopList !! 0 @?= Dec
            loopList !! 1 @?= Right
            loopList !! 2 @?= Inc
            loopList !! 3 @?= Left

            1 @?= 1

    describe "running" $ do

        let dataWithHInFirst = dataModifyValue emptyData (const 72)

        it "simple programs" $ do
            run [Print] "" dataWithHInFirst @?= "Hdone!\n"

        it "simple loop" $ do
            let tail = [loop, Right, Print]
                loop = Loop $ Dec : Right : Inc : Left : tail
            run (Print : tail) "" dataWithHInFirst @?= "HHdone!\n"
