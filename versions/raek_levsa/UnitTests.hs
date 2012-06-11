import Brainfuck
import Test.Hspec.HUnit
import Test.Hspec.Monadic
import Test.HUnit
import qualified Data.Map as M

main = hspecX $ do

    describe "the parser" $ do
        it "recognized supported operations" $ do
            parseProgram "<>+-.," @?=
                [MLeft, MRight, Inc, Dec, Out, In]

    describe "the operation interpreter" $ do
        it "can execute <" $ do
            executeOp MLeft (Machine [] 123 M.empty) @?=
                ((Machine [] 122 M.empty), Nothing)
        it "can execute >" $ do
            executeOp MRight (Machine [] 123 M.empty) @?=
                ((Machine [] 124 M.empty), Nothing)
        it "can execute +" $ do
            executeOp Inc (Machine [] 123 (M.singleton 123 456)) @?=
                ((Machine [] 123 (M.singleton 123 457)), Nothing)
        it "can execute + on untouched cell" $ do
            executeOp Inc (Machine [] 123 M.empty) @?=
                ((Machine [] 123 (M.singleton 123 1)), Nothing)
        it "can execute -" $ do
            executeOp Dec (Machine [] 123 (M.singleton 123 456)) @?=
                ((Machine [] 123 (M.singleton 123 455)), Nothing)
        it "can execute ." $ do
            executeOp In (Machine [789] 123 (M.singleton 123 456)) @?=
                ((Machine [] 123 (M.singleton 123 789)), Nothing)
        it "can execute . on untouched" $ do
            executeOp In (Machine [789] 123 M.empty) @?=
                ((Machine [] 123 (M.singleton 123 789)), Nothing)
        it "can execute ," $ do
            executeOp Out (Machine [] 123 (M.singleton 123 456)) @?=
                ((Machine [] 123 (M.singleton 123 456)), Just 456)
        it "can execute ," $ do
            executeOp Out (Machine [] 123 M.empty) @?=
                ((Machine [] 123 M.empty), Just 0)

    describe "the interpreter" $ do
        it "interprets the empty program" $ do
            executeProgram (initialMachine []) [] @?= []
        it "interprets a program with one inc operation" $ do
            executeProgram (initialMachine []) [Inc] @?= []
        it "interprets a program with one out operation" $ do
            executeProgram (initialMachine []) [Out] @?= [0]
        it "interprets a program from a string" $ do
            execute' ",+." "a" @?= "b"
