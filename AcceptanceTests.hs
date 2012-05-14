import System.Exit
import System.Process

runTest :: String -> String -> String -> String -> IO Bool
runTest name program input expectedOutput = do
    (exit, stdout, stderr) <- readProcessWithExitCode "./dist/Main" [program] input
    let ex = (expectedOutput ++ "done!\n")
    if stdout == ex
        then putStrLn ("success: " ++ name) >>
             return True
        else putStrLn ("failure: " ++ name) >>
             putStrLn ("  expected: " ++ ex) >>
             putStrLn ("   but got: " ++ stdout) >>
             return False

runTests :: [IO Bool] -> IO ()
runTests tests = do
    results <- mapM id tests
    if any (==False) results
        then exitFailure
        else exitSuccess

main = runTests

    [ runTest "print single char a"
        "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++."
        ""
        "a"

    , runTest "hello world"
        "test_programs/hello_world.bf"
        ""
        "Hello World!\n"

    , runTest "single character echo"
        "test_programs/single_char_echo.bf"
        "g"
        "g"

    ]
