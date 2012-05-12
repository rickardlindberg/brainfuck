import System.Process

runTest :: String -> String -> String -> String -> IO ()
runTest name program input expectedOutput = do
    (exit, stdout, stderr) <- readProcessWithExitCode "runhaskell" ["Brainfuck.hs", program] input
    let ex = (expectedOutput ++ "done!\n")
    if stdout == ex
        then putStrLn ("success: " ++ name)
        else putStrLn ("failure: " ++ name) >>
             putStrLn ("  expected: " ++ ex) >>
             putStrLn ("   but got: " ++ stdout)

main = do

    runTest "print single char a"
        "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++."
        ""
        "a"

    runTest "hello world"
        "test_programs/hello_world.bf"
        ""
        "Hello World!\n"

    runTest "single character echo"
        ",."
        "g"
        "g"