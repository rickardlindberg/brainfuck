import System.Exit
import System.Process

data Test = Test
    { name           :: String
    , program        :: String
    , input          :: String
    , expectedOutput :: String
    }

runTest :: Test -> IO Bool
runTest test = do
    (exit, stdout, stderr) <- readProcessWithExitCode "./dist/Main" [program test] (input test)
    let ex = (expectedOutput test ++ "done!\n")
    if stdout == ex
        then putStrLn ("success: " ++ name test) >>
             return True
        else putStrLn ("failure: " ++ name test) >>
             putStrLn ("  expected: " ++ ex) >>
             putStrLn ("   but got: " ++ stdout) >>
             return False

runTests :: [Test] -> IO ()
runTests tests = do
    results <- mapM runTest tests
    if any (==False) results
        then exitFailure
        else exitSuccess

main = runTests

    [ Test
        { name           = "print single char a"
        , program        = "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++."
        , input          = ""
        , expectedOutput = "a"
        }

    , Test
        { name           = "print single char a with loop"
        , program        = "+++++[>+++++++++++++++++++<-]>++."
        , input          = ""
        , expectedOutput = "a"
        }

    , Test
        { name           = "hello world"
        , program        = "test_programs/hello_world.bf"
        , input          = ""
        , expectedOutput = "Hello World!\n"
        }

    , Test
        { name           = "single character echo"
        , program        = "test_programs/single_char_echo.bf"
        , input          = "g"
        , expectedOutput = "g"
        }

    , Test
        { name           = "echo until q"
        , program        = "test_programs/echo_until_q.bf"
        , input          = "rewq"
        , expectedOutput = "rew"
        }

    ]
